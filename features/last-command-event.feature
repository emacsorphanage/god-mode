Feature: Prefix arguments
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "abcdefghijklmnopqrstuvwxyz"
    And I have god-mode on
    And I go to line "1"

  Scenario: M-4 C-d does delete 4 times
    When I press "M-4 C-d"
    Then the buffer's contents should be "efghijklmnopqrstuvwxyz"

  Scenario: g4d does delete 4 times
    When I press "g4d"
    Then the buffer's contents should be "efghijklmnopqrstuvwxyz"
