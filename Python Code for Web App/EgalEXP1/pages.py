from ._builtin import Page, WaitPage
from .models import Constants

import random

class InstructionsTemp(Page):
    pass



class Endow(Page):
    pass


class Offer(Page):
    form_model = 'player'
    form_fields = ['choice']

    def before_next_page(self):
        if self.player.choice == 'Choice 1 (50,50)':
            self.player.kept = 50
            self.player.payoff = 50
        else:
            self.player.kept = 75
            self.player.payoff = 75


class Results(Page):
   pass


page_sequence = [
    InstructionsTemp,
    Offer,
    Results
]
