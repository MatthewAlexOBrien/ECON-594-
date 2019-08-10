from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random


class Constants(BaseConstants):
    name_in_url = 'dictatorEXP'
    players_per_group = None
    num_rounds = 1

    instructions_template = 'EgalEXP1/instructions.html'


class Subsession(BaseSubsession):
    pass



class Group(BaseGroup):
    pass



class Player(BasePlayer):

    choice = models.StringField(
        label='Income split choice',
        choices=['Choice 1 (50,50)', 'Choice 2 (75, 150)'],
        widget=widgets.RadioSelect
    )

    kept = models.CurrencyField(
        label='Amount earned from split'
    )

    def set_payoffs(self):
        p1 = self.get_player_by_id(1)

        if self.choice == 'Choice 1 (50,50)':
            p1.kept = 50
            p1.payoff = 50
        else:
            p1.kept = 75
            p1.payoff = 75
