Feature: C- commands
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert:
    """
    laziness
    impatience
    hubris
    """
    Then I have god-mode on

  Scenario: map k into kill-line
    Given I go to line "1"
    And I press "kk"
    Then the buffer's contents should be:
    """
    impatience
    hubris
    """

  Scenario: map / into undo
    Given I go to line "1"
    And I press "kk"
    And I press "//"
    Then the buffer's contents should be:
    """
    laziness
    impatience
    hubris
    """
