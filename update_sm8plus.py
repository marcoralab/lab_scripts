#!/usr/bin/env python3

import re
from pathlib import Path
import argparse

def update_walltime_to_runtime(lines, fname):
    """
    Replace 'walltime = H:00' with 'runtime = Hh' and 0:MM with MMm
    """
    def repl(match):
        indent = match.group(1)
        hours = int(match.group(2))  # convert to int to remove leading zeros
        minutes = int(match.group(3))
        comma = match.group(4) or ""
        comment = match.group(5)
        if hours > 0 and minutes > 0:
            print(f"Warning: Non-zero minutes in time in {fname}. Please convert to hours.")
            ret = None
        elif hours > 0:
            ret = f'{indent}runtime = "{hours}h"{comma}'
        else:
            ret = f'{indent}runtime = "{minutes}m"{comma}'
        if ret and comment:
            return f'{ret} {comment}'
        elif ret:
            return ret
        else:
            return match.group(0)
    
    wtre = re.compile(r'''^(\s+)walltime\s*=\s+["'](\d+):(\d\d)["']\s*(,)?\s*(#.*)?$''')
    
    lines_out = []
    for lnum, line in enumerate(lines):
        if re.search(r"walltime", line):
            edited = wtre.sub(repl, line)
            if edited == line:
                print(f"Warning: Line {lnum + 1} of {fname} ({line}) not changed")
            lines_out.append(edited)
        else:
            lines_out.append(line)
    return lines_out

def repair_runtime(lines, fname):
    """
    Replace 'runtime: ".+"' with 'runtime = ".+"'
    """
    def repl(match):
        indent = match.group(1)
        runtime = match.group(2)
        line_end = match.group(3) or ""
        return f'{indent}runtime = "{runtime}"{line_end}'
    
    wtre = re.compile(r'''^(\s+)runtime: "([^"]+)"(.+)?$''')
    
    lines_out = []
    for lnum, line in enumerate(lines):
        if re.search(r"runtime", line):
            edited = wtre.sub(repl, line)
            print(f"Repairing runtime in line {lnum + 1} of {fname}: {line.strip()} -> {edited.strip()}")
            if edited == line:
                print(f"Warning: Line {lnum + 1} of {fname} ({line}) not changed")
            lines_out.append(edited)
        else:
            lines_out.append(line)
    return lines_out

def update_storage_remote_calls(lines, fname, always_dummy):
    """
    Replace 'HTTP.remote(' with 'storage.http(' and 'FTP.remote(' with 'storage.ftp('.
    If dummyprovider is used, retain iconnect logic; otherwise, clean up unnecessary blocks.
    """
    if not any("HTTP.remote" in line or "FTP.remote" in line for line in lines):
        return lines

    dprov = any("dummyprovider" in line for line in lines)
    print(f"File {fname} uses dummyprovider: {dprov}")
    dprov = always_dummy or dprov

    def normalize(line):
        return line.strip()
    
    # These are blocks to remove, each defined by a list of *starting lines*
    block_starts_to_remove = [
        ["iconnect = iconnect and not ('nointernet' in config and config['nointernet'])"],
        ["try:", "response = urlopen('https://www.google.com/', timeout=10)"],
        ['if snakemake.__version__.startswith("7."):',
         'from snakemake.remote.FTP import RemoteProvider as FTPRemoteProvider'],
        ["from snakemake.remote.FTP import RemoteProvider as FTPRemoteProvider"],
        ["from snakemake.remote.HTTP import RemoteProvider as HTTPRemoteProvider"],
        ["from urllib.request import urlopen"],
        ["FTP = FTPRemoteProvider() if iconnect else dummyprovider"],
        ["HTTP = HTTPRemoteProvider() if iconnect else dummyprovider"],
        ["class dummyprovider:"],
        ["if iconnect and not ('nointernet' in config and config['nointernet']):"]
    ]
    
    # Normalize block start patterns
    normalized_block_starts = [
        [normalize(line) for line in block] for block in block_starts_to_remove
    ]
    
    continuation_keywords = ("else:", "elif ", "except", "finally")
    
    def indentation(line):
        return len(line) - len(line.lstrip(' '))
    
    def get_full_block(lines, start_idx):
        """Return the end index of a compound block including else/except/finally."""
        start_indent = indentation(lines[start_idx])
        i = start_idx + 1
        while i < len(lines):
            line = lines[i]
            stripped = line.strip()
    
            if stripped == "":
                i += 1
                continue
    
            current_indent = indentation(line)
    
            # Check for continuation blocks at the same indent level
            if current_indent == start_indent and any(stripped.startswith(k) for k in continuation_keywords):
                # It's part of the block (like `else:` or `except`)
                # Recurse to find the end of this sub-block
                sub_end = get_full_block(lines, i)
                i = sub_end
            elif current_indent <= start_indent:
                break
            else:
                i += 1
    
        return i

    # Main removal logic
    out1 = []
    i = 0
    while i < len(lines):
        matched = False
        for pattern in normalized_block_starts:
            pat_len = len(pattern)
            if i + pat_len <= len(lines):
                segment = [normalize(l) for l in lines[i:i+pat_len]]
                if segment == pattern:
                    block_end = get_full_block(lines, i)
                    i = block_end
                    matched = True
                    break
        if not matched:
            out1.append(lines[i])
            i += 1

    # Step 2: Modify or insert code based on dummyprovider
    out = []
    inserted_connection_block = False

    for line in out1:
        if dprov and not inserted_connection_block and re.search(r"from urllib.error import URLError", line):
            print(f"Inserting connection block into {fname}")
            out.append(
                "from urllib.request import urlopen\n"
                "from urllib.error import URLError\n\n"
                "iconnect = False\n"
                "if not ('nointernet' in config and config['nointernet']):\n"
                "    try:\n"
                "        response = urlopen('https://www.google.com/', timeout=10)\n"
                "        iconnect = True\n"
                "    except URLError as ex:\n"
                "        pass\n"
                ""
                "ftp = storage.ftp if iconnect else lambda x: x\n"
                "http = storage.http if iconnect else lambda x: x\n"
            )
            inserted_connection_block = True
            continue

        line = (
            line.replace('HTTP.remote(', 'http(')
                .replace('FTP.remote(', 'ftp(')
                .replace(', immediate_close=True)', ')')
                .replace(', allow_redirects=True)', ')')
        )

        if not dprov:
            line = line.replace('http(', 'storage.http(').replace('ftp(', 'storage.ftp(')

        out.append(line)

    return out

def update_mem_mb(lines, fname):
    """
    Update mem_mb lines inside each rule block based on threads.
    - If numeric: multiply by threads.
    - If symbolic: append * threads.
    - If no threads: assume 1.
    """
    re_mem = re.compile(r'^(\s*mem_mb)(\s*=\s*)(\S+?)(,?\s*(#.*)?)$')
    threads = 1
    out = []
    for lnum, line in enumerate(lines):
        if re.search(r"^\s*rule\s+", line):
            threads = 1
            out.append(line)
        elif re.search(r"mem_mb", line):
            match_mem = re_mem.match(line)
            if match_mem:
                prefix, eqsign, value, lineend, comment = match_mem.groups()
                if re.match(r'^\d+$', value):  # numeric
                    new_value = str(int(value) * threads)
                elif '*' in value:  # already has multiplication
                    new_value = value
                    print(f"Warning: Line {lnum} of {fname} ({line}) not changed. Multiplication already present.")
                else:  # symbolic
                    new_value = f'{value} * {threads}'
            else:
                print(f"Warning: Line {line} of {fname} ({line}) not changed")
            out.append(f"{prefix} = {new_value}{lineend}")
        else:
            match_threads = re.search(r"threads:\s+(\d+)", line)
            if match_threads:
                threads = int(match_threads.group(1))
            out.append(line)
    return out

def specify_min_ver(lines):
    """
    Add version check to Snakefile.
    """

    import_line = "from snakemake.utils import min_version"

    mvpat = re.compile(r"""min_version\(["'0-9.\s]+\)""")

    add_version = True
    if any(mvpat.search(line) for line in lines): # update the version
        lines = [mvpat.sub('min_version("8.0")', line) for line in lines]
        print("Updated version check in Snakefile")
        if any(import_line in line for line in lines):
            return lines
        add_version = False
    
    # add the import line after the last import within 20 lines of the start
    first_twenty = lines[:min(20, len(lines) - 1)]
    import_dex_existing = [i for i, line in enumerate(first_twenty) if 'import' in line]
    if import_dex_existing:
        import_dex = import_dex_existing[-1] + 1
    else: # insert after any initial comments; detect #, """ or '''
        import_dex = next((i for i, line in enumerate(first_twenty) if re.search(r'^\s*["\']{3}', line)), 0)
        if import_dex == 0:
            import_dex = next((i for i, line in enumerate(first_twenty) if re.search(r'^\s*#', line)), 0)
    lines.insert(import_dex, import_line + '\n')
    if add_version:
        lines.insert(import_dex + 1, 'min_version("8.0")\n')
        print("Added version check to Snakefile")
    return lines

def detect_sm8plus(path):
    """
    Returns True if the Snakefile contains a min_version call with version >= 8.0.
    Ignores any content after a '#' character on each line (inline comments).
    Returns False if min_version is not present or version is less than 8.
    """
    mvpat = re.compile(r"""min_version\(\s*['"]([0-9.]+)['"]\s*\)""")
    
    with path.open('r', encoding='utf-8') as f:
        lines = f.readlines()

    for line in lines:
        code_part = line.split('#', 1)[0]  # Remove everything after '#' (inline comment)
        match = mvpat.search(code_part)
        if match:
            version_str = match.group(1)
            try:
                major = int(version_str.split('.')[0])
                return major >= 8
            except ValueError:
                return False
    return False

def cluster_yaml_to_profile():
    """
    Convert cluster.yaml to a Snakemake profile.
    """
    import os

    def remove_empty_values(d):
        """
        Remove empty categories or items from config
        """
        result = {}
        for k, v in d.items():
            if isinstance(v, dict):
                cleaned = remove_empty_values(v)
                if cleaned:  # skip if empty dict
                    result[k] = cleaned
            elif v != "":  # skip empty strings
                result[k] = v
        return result

    try:
        import yaml
    except ImportError:
        print("Warning: PyYAML is not installed. Please install it to convert cluster.yaml to Snakemake profile.")
        return

    if not os.path.exists('cluster.yaml'):
        print("Warning: No cluster.yaml file found. Skipping conversion.")
        return

    if os.path.exists('profiles/default/config.yaml'):
        print("Warning: Profile directory already exists. Skipping conversion.")
        return
    else:
        os.makedirs('profiles/default', exist_ok=True)

    with open('cluster.yaml', 'r') as file:
        cluster_config_original = yaml.safe_load(file)
    
    cluster_config = {'set-resources': {}, 'set-threads': {}, 'default-resources': {}}
    
    wtre = re.compile(r'(\d+):(\d\d)')

    add_key_if_missing = lambda k, o: {} if not k in o else o[k]

    if "__default__" in cluster_config_original and "cores" in cluster_config_original["__default__"]:
        default_cores = cluster_config_original["__default__"]["cores"]
        if default_cores != 1:
            print("Warning: I don't know how to convert default threads. Skipping.")
    else:
        default_cores = 1

    for key in cluster_config_original.keys():
        if 'time' in cluster_config_original[key]:
            cluster_config['set-resources'][key] = add_key_if_missing(key, cluster_config['set-resources'])
            try:
                time_str = cluster_config_original[key].pop('time')
                time = wtre.match(time_str)
                runtime_hr = int(time.group(1))
                runtime_min = int(time.group(2))
                if runtime_min > 0 and runtime_hr > 0:
                    print("Warning: Non-zero minutes in time. Please convert to hours.")
                elif runtime_min > 0:
                    cluster_config['set-resources'][key]['runtime'] = f'{runtime_min}m'
                else:
                    cluster_config['set-resources'][key]['runtime'] = f'{runtime_hr}h'
            except:
                print(f"Warning: Unable to parse time '{time_str}' for key '{key}'. Skipping.")
                cluster_config_original[key]['time'] = time_str
        if 'cores' in cluster_config_original[key]:
            cores = int(cluster_config_original[key].pop('cores'))
            if key != '__default__':
                cluster_config['set-threads'][key] = cores
        else:
            cores = default_cores
        if 'mem' in cluster_config_original[key]:
            mem = int(cluster_config_original[key].pop('mem')) * cores
            cluster_config['set-resources'][key] = add_key_if_missing(key, cluster_config['set-resources'])
            cluster_config['set-resources'][key]['mem_mb'] = mem
        if 'resources' in cluster_config_original[key]:
            cluster_config['set-resources'][key] = add_key_if_missing(key, cluster_config['set-resources'])
            cluster_config['set-resources'][key]['lsf_extra'] = cluster_config_original[key].pop('resources')
        if 'queue' in cluster_config_original[key]:
            cluster_config['set-resources'][key] = add_key_if_missing(key, cluster_config['set-resources'])
            cluster_config['set-resources'][key]['lsf_queue'] = cluster_config_original[key].pop('queue')
        if 'project' in cluster_config_original[key]:
            cluster_config['set-resources'][key]['lsf_project'] = cluster_config_original[key].pop('project')
        if 'partition' in cluster_config_original[key]:
            print("Info: partition is no longer needed on Minerva. Skipping.")
            discard = cluster_config_original[key].pop('partition')
    
    if '__default__' in cluster_config['set-resources']:
        cluster_config['default-resources'] = cluster_config['set-resources'].pop('__default__')

    cluster_config = remove_empty_values(cluster_config)
    cluster_config_original = remove_empty_values(cluster_config_original)

    if cluster_config:
        if cluster_config_original:
            print("Warning: Some keys in cluster.yaml were not converted. Please manually convert.")
            print("    Unconverted config:")
            for line in yaml.dump(cluster_config_original).splitlines():
                print("    " + line)

        with open('profiles/default/config.yaml', 'w') as file:
            yaml.dump(cluster_config, file)

        print("Converted cluster.yaml to Snakemake profile.")
    else:
        print("Warning: No keys in cluster.yaml were converted. Please manually convert.")


def process_file(path, dummyprovider=False, keep_mem=False, label_only=False, repair_runtime_bug=False):
    """
    Process a single file: read, transform, write in-place,
    preserving trailing newline if present.
    """

    procd = '### Processed by update_sm8plus.py for Snakemake 8+ ###'

    # get the file name without the path
    fname = path.name
    snakefile = fname == 'Snakefile'

    with path.open('r', encoding='utf-8') as f:
        orig = f.readlines()
    
    if label_only:
        content = orig
    elif repair_runtime_bug:
        content = repair_runtime(orig, path)
    else:
        if any(procd in line or "runtime" in line for line in orig):
            print(f"File {path} already processed. Skipping.")
            return
        
        content = update_walltime_to_runtime(orig, path)
        content = content if keep_mem else update_mem_mb(content, path) 
        content = update_storage_remote_calls(content, path, dummyprovider)

    already_labeled = any(procd in line or "runtime" in line for line in orig)
    labelme = not already_labeled and (label_only or content != orig)

    if snakefile or labelme or content != orig:
        if labelme:
            content.append(f'\n{procd}\n')
        if snakefile:
            content = specify_min_ver(content)
        with path.open('w', encoding='utf-8') as f:
            f.writelines(content)
        print(f"Updated {path}")
    else:
        print(f"No changes to {path}")


def find_target_files():
    """
    Recursively find all Snakefile and *.smk files.
    """
    return list(Path('.').rglob('Snakefile')) + list(Path('.').rglob('*.smk'))


def main():
    parser = argparse.ArgumentParser(
        prog='update_sm8plus',
        description='Update Snakemake files for SM8+ compatibility')
    parser.add_argument(
        '-d', '--dummyprovider',
        help='Always use dummy provider if no internet connection',
        action='store_true')
    parser.add_argument(
        '-m', '--keep-mem',
        help='Keep old mem_mb lines',
        action='store_true')
    parser.add_argument(
        '-l', '--label-only',
        help='Only add label to Snakefile, do not modify it (other than min_version)',
        action='store_true')
    parser.add_argument(
        '-p', '--profile',
        help='Convert cluster.yaml to Snakemake profile',
        action='store_true')
    parser.add_argument(
        '--repair-runtime-bug',
        help='Repair runtime bug from previous version of script',
        action='store_true')
    args = parser.parse_args()
    files = find_target_files()
    snakefile = [path for path in files if path.name == 'Snakefile']
    if len(snakefile) > 1:
        print("Warning: More than one Snakefile found. Only the first will version checked.")
    elif len(snakefile) == 0:
        print("Warning: No Snakefile found. Version check not performed.")
    elif not args.repair_runtime_bug and detect_sm8plus(snakefile[0]):
        raise ValueError("Snakefile already requires Snakemake 8+. No changes made.")
    if args.profile:
        cluster_yaml_to_profile()
    for path in files:
        process_file(path, args.dummyprovider, args.keep_mem, args.label_only, args.repair_runtime_bug)

if __name__ == '__main__':
    main()
