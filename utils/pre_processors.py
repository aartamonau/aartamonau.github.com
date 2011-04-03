import re

from os import path
from datetime import datetime
from django.conf import settings

class DateInjector(object):
    DATED_NAME = re.compile("(\d{4}-\d{2}-\d{2})-(\d{2}:\d{2})?.+")

    @staticmethod
    def process(folder, params):
        context = settings.CONTEXT

        site = context['site']
        node = params['node']

        for page in node.walk_pages():
            match = DateInjector.DATED_NAME.match(page.page_name)
            if match is None:
                continue

            try:
                date = datetime.strptime(match.groups()[0], "%Y-%m-%d")

                if match.groups()[1] is not None:
                    time = datetime.strptime(match.groups()[1], "%H:%M")
                    date = datetime.combine(date, time.time())
            except Exception, e:
                print 'Failed to extract date from page "%s": %s' % \
                    (page.page_name, str(e))
                continue

            setattr(page, 'created', date)
