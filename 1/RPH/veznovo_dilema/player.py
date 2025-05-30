class MyPlayer:
    """This player plays a Tit for Tat strategy"""

    def __init__(self, payoff_matrix, number_of_iterations=None):
        self.payoff_matrix = payoff_matrix
        self.number_of_iterations = number_of_iterations
        self.my_history = []
        self.opponent_history = []

    def move(self):
        both_cooperate = self.payoff_matrix[0][0][0]  # [0] = COOPERATE
        both_defect = self.payoff_matrix[1][1][0]  # [1] = DEFECT
        i_defect = self.payoff_matrix[1][0][0]
        i_cooperate = self.payoff_matrix[0][1][0]
        if self.my_history == [] and i_defect >= 10 * both_cooperate:
            return True
        elif self.my_history == [] and i_cooperate >= 10 * both_defect:
            return False
        # no dilemma and dominant strategy is to defect
        elif both_cooperate <= both_defect and i_cooperate <= both_defect \
                and i_defect <= both_defect:
            return True
        # no dilemma and dominant strategy is to cooperate
        elif both_defect <= both_cooperate and i_cooperate <= both_cooperate \
                and i_defect <= both_cooperate:
            return False
        elif self.my_history == [] and both_cooperate <= both_defect:
            return True
        elif self.my_history == [] and both_cooperate > both_defect:
            return False
        else:
            return self.opponent_history[-1]

    def record_last_moves(self, my_last_move, opponent_last_move):
        self.my_history.append(my_last_move)
        self.opponent_history.append(opponent_last_move)


if __name__ == "__main__":
    p1 = MyPlayer(((4, 4), (1, 6)), ((6, 1), (2, 2)))
    print(p1.move())
