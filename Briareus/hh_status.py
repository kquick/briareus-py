#! /usr/bin/env nix-shell
#! nix-shell -i "python3.7 -u" -p "python37.withPackages(pp: with pp; [  attrs ])"

import argparse

def text_formatter(repfile):
    lines = len([line for line in repfile])
    print('Text status from a', lines, ' line report')

def html_formatter(repfile):
    lines = len([line for line in repfile])
    print('HTML status from a', lines, ' line report')

def main():
    supported_formats = { 'text': text_formatter,
                          'html': html_formatter,
    }

    parser = argparse.ArgumentParser(
        description='Show status output from Briareus build report.',
        epilog=('This tool simply formats the specified build report from the Briareus'
                ' hh (hundred hander) tool; it does not generate build configurations,'
                ' obtain build results, or perform any other actions.'),
        prog='hh_status')
    parser.add_argument(
        '--format', '-f', default='text', choices=list(supported_formats.keys()),
        help='Type of status output to generate',
    )
    parser.add_argument(
        'INPUT_REPORT', type=argparse.FileType('r'),
        help="Briareus report file used as input (use '-' to read from stdin).")
    args = parser.parse_args()

    supported_formats[args.format](args.INPUT_REPORT)

if __name__ == "__main__":
    main()
