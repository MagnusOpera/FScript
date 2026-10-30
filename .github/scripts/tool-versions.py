#!/usr/bin/env python3
"""Read the shared, flat tool inventory from WORKSPACE without needing Terrabuild."""
import argparse
import json
import re
from pathlib import Path


def read_versions(workspace):
    source = Path(workspace).read_text()
    workspace_block = re.search(r'\bworkspace\s*\{([^}]+)\}', source)
    versions_block = re.search(r'\bversions\s*=\s*\{([^}]+)\}', source)
    if not workspace_block or not versions_block:
        raise ValueError('WORKSPACE must declare workspace and locals.versions blocks')

    def fields(block):
        return dict(re.findall(r'^\s*(\w+)\s*[:=]\s*"([^"\n]+)"', block, re.MULTILINE))

    workspace_values = fields(workspace_block.group(1))
    versions = fields(versions_block.group(1))
    versions['terrabuild'] = workspace_values['version']
    versions['workspace_id'] = workspace_values['id']
    for name in ('terrabuild', 'dotnet_sdk', 'node', 'pnpm', 'vsce', 'ovsx'):
        if not re.fullmatch(r'[0-9][0-9A-Za-z.+_-]*', versions.get(name, '')):
            raise ValueError(f'Missing or invalid tool version: {name}')
    if not re.fullmatch(r'[0-9a-f]{24}', versions['workspace_id']):
        raise ValueError('Invalid Insights workspace ID')
    return versions


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--workspace', default='WORKSPACE')
    parser.add_argument('--github-output')
    args = parser.parse_args()
    versions = read_versions(args.workspace)
    if args.github_output:
        with open(args.github_output, 'a') as output:
            for name, value in versions.items():
                print(f'{name}={value}', file=output)
    else:
        print(json.dumps(versions, indent=2))
