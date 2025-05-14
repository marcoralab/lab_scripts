#!/usr/bin/env python3

import re
from pathlib import Path
import argparse

def update_walltime_to_runtime(lines, fname):
    """
    Replace 'walltime: H:00' with 'runtime: Hh'
    """
    def repl(match):
        indent = match.group(1)
        hours = int(match.group(2))  # convert to int to remove leading zeros
        return f'{indent}runtime: "{hours}h"\n'
    
    wtre = re.compile(r'''^(\s+)walltime\s*=\s+["'](\d+):00["']\s*$''')
    
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

    blocks_to_remove = [
        [
            "from snakemake.remote.FTP import RemoteProvider as FTPRemoteProvider\n"
        ],
        [
            "from snakemake.remote.HTTP import RemoteProvider as HTTPRemoteProvider\n"
        ],
        [
            "from urllib.request import urlopen\n"
        ],
        [
            "try:\n",
            "    response = urlopen('https://www.google.com/', timeout=10)\n",
            "    iconnect = True\n",
            "except URLError as ex:\n",
            "    iconnect = False\n"
        ],
        [
            "class dummyprovider:\n",
            "    def remote(string_, allow_redirects=\"foo\", immediate_close=\"bar\"):\n",
            "        return string_\n"
        ],
        [
            "if iconnect and not ('nointernet' in config and config['nointernet']):\n",
            "    FTP = FTPRemoteProvider()\n",
            "    HTTP = HTTPRemoteProvider()\n",
            "else:\n",
            "    FTP = dummyprovider\n",
            "    HTTP = dummyprovider\n"
        ]
    ]

    # Step 1: Remove the blocks
    out1 = []
    i = 0
    while i < len(lines):
        matched = False
        for block in blocks_to_remove:
            blen = len(block)
            if lines[i:i+blen] == block:
                i += blen
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
            )
            inserted_connection_block = True
            continue

        line = (
            line.replace('HTTP.remote(', 'http(')
                .replace('FTP.remote(', 'ftp(')
                .replace(', immediate_close=True)', ')')
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


def process_file(path, dummyprovider=False):
    """
    Process a single file: read, transform, write in-place,
    preserving trailing newline if present.
    """
    with path.open('r', encoding='utf-8') as f:
        content = f.readlines()

    content = update_walltime_to_runtime(content, path)
    content = update_mem_mb(content, path)
    content = update_storage_remote_calls(content, path, dummyprovider)

    with path.open('w', encoding='utf-8') as f:
        f.writelines(content)


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
    args = parser.parse_args()
    files = find_target_files()
    for path in files:
        process_file(path, args.dummyprovider)


if __name__ == '__main__':
    main()
