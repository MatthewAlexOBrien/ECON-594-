from ._builtin import Page, WaitPage
from .models import Constants

import random

class Introduction(Page):
    pass

class InstructionsTemp(Page):

    def before_next_page(self):
        self.player.initial = random.choice(Constants.income_list)
        self.player.partner = random.choice(Constants.income_list)

class Endow(Page):
    pass


class Offer(Page):
    form_model = 'player'
    form_fields = ['kept']

    def before_next_page(self):
        self.player.payoff = self.player.kept + self.player.initial


class Results(Page):
    pass



page_sequence = [
    Introduction,
    InstructionsTemp,
    Endow,
    Offer,
    Results
]
