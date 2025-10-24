import IPython
from tabulate import tabulate


class OrgFormatter(IPython.core.formatters.BaseFormatter):
    def __call__(self, obj):
        # don't convert strings into tables
        if isinstance(obj, (str, bytes)):
            return None

        try:
            return tabulate(obj, headers='keys', tablefmt='orgtbl', showindex='always')
        except:
            return None

ip = get_ipython()
ip.display_formatter.formatters['text/org'] = OrgFormatter()
