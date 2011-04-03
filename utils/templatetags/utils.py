from itertools import ifilter

from django.template import Library

register = Library()

@register.filter
def sort_by(items, field):
    components = field.split('.')
    def get(item):
        current = item
        for component in components:
            current = getattr(current, component)

        return current

    return sorted(items, key=get)
