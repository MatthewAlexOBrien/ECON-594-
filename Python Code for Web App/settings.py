
import os
from os import environ



# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = {
    'real_world_currency_per_point': 1.00,
    'participation_fee': 0.00,
    'doc': "",
}

# the environment variable OTREE_PRODUCTION controls whether Django runs in
# DEBUG mode. If OTREE_PRODUCTION==1, then DEBUG=False
if environ.get('OTREE_PRODUCTION') not in {None, '', '0'}:
    DEBUG = False
    APPS_DEBUG = False
else:
    DEBUG = True
    APPS_DEBUG = True   # will set a debug variable to true in the template files

SESSION_CONFIGS = [

#    {
#        'name': 'real_effort',
#        'display_name': "Effort",
#        'num_demo_participants': 3,
#        'app_sequence': ['real_effort'],
#    },
     {
        'name': 'dictator',
        'display_name': "Dictator Game",
        'num_demo_participants': 1,
        'app_sequence': ['dictator1', 'EgalEXP1', 'survey', 'payment_info'],
     },
#    {
#        'name': 'public_goods_simple',
#        'display_name': "Public Goods (simple version from tutorial)",
#        'num_demo_participants': 3,
#        'app_sequence': ['public_goods_simple'],
#    },

]
# see the end of this file for the inactive session configs


# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [

    {
        'name': 'live_demo',
        'display_name': 'Matts Dictator Experiment',
    },
]


AUTH_LEVEL = environ.get('DEMO')

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')



DEMO_PAGE_INTRO_HTML = """
Here are some oTree games.
"""

# don't share this with anybody.
SECRET_KEY = '5s6^2dt8jxz40z13xuzd$q3z3k8rw2v7eyf2$7hbva@y!c*j%s'

# if an app is included in SESSION_CONFIGS, you don't need to list it here
INSTALLED_APPS = ['otree']

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

