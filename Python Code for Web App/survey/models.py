from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random


class Constants(BaseConstants):
    name_in_url = 'survey'
    players_per_group = None
    num_rounds = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):

    age = models.IntegerField(
        label='What is your age?',
        min=13, max=125)

    gender = models.StringField(
        choices=['Male', 'Female', 'Other', 'Prefer Not To Disclose'],
        label='What is your gender?',
        widget=widgets.RadioSelect)

    education = models.StringField(
        choices=['University/College Graduate Degree', 'University/College Undergraduate Degree',
                 'University/College Diploma', 'Some University/College Classes',
                 'High School', 'Some High School Classes', 'Other', 'Prefer Not To Disclose'],
        label='What is your highest level of education completed?',
        widget=widgets.RadioSelect)

    ethnicity = models.StringField(
        choices=['White', 'Black or African American', 'Hispanic or Latino', 'Asian',
                 'Pacific Islander', 'Arab or Middle Eastern', 'Native North American',
                 'Other', 'Prefer Not To Disclose'],
        label='What is your ethnicity?',
        widget=widgets.RadioSelect)

    maritalstatus = models.StringField(
        choices=['Single, never married', 'Married or domestic partnership', 'Widowed',
                 'Divorced', 'Separated', 'Other', 'Prefer Not To Disclose'],
        label='What is your marital status?',
        widget=widgets.RadioSelect)

    employment = models.StringField(
        choices=['Employed for wages', 'Self-Employed', 'Unemployed looking for work',
                 'Unemployed not looking for work', 'Student', 'Military',  'Retired',
                 'Unable to work', 'Other', 'Prefer Not To Disclose'],
        label='What is your employment status',
        widget=widgets.RadioSelect)

    income = models.IntegerField(
        label='Roughly, what is your annual income in American dollars? (Please enter 0 if you prefer not to disclose)',
        min=0)

