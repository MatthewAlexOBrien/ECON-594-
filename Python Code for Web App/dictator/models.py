from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random


doc = """
One player decides how to divide a certain amount between himself and the other
player.

See: Kahneman, Daniel, Jack L. Knetsch, and Richard H. Thaler. "Fairness
and the assumptions of economics." Journal of business (1986):
S285-S300.

"""


class Constants(BaseConstants):
    name_in_url = 'dictator'
    players_per_group = None
    num_rounds = 1

    instructions_template = 'dictator/instructions.html'

    # Initial amount allocated to the dictator
    endowment = c(100)

    # Randomize initial endowment
    income_list = [c(10), c(100), c(300)]
    initial_income = random.choice(income_list)

    # Randomize partner's endowment
    partner_income = random.choice(income_list)


class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Player(BasePlayer):

    initial = models.CurrencyField(
        label='What is the initial endowment?'
    )
    partner = models.CurrencyField(
        label='What is my partners endowment'
    )
    kept = models.CurrencyField(
        label='Income split choice'
    )

    def set_payoffs(self):
        for p in self.get_players():
            p.kept = self.kept
            p.payoff = self.kept + self.initial