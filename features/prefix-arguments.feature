Feature: Prefix arguments
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "abcdefghijklmnopqrstuvwxyz"
    And I have god-mode on
    And I go to line "1"

  Scenario: C-u C-d does delete 4 times
    When I press "C-u C-d"
    Then the buffer's contents should be "efghijklmnopqrstuvwxyz"

  Scenario: ud does delete 4 times
    When I press "ud"
    Then the buffer's contents should be "efghijklmnopqrstuvwxyz"

  Scenario: C-u C-u C-d does delete 16 times
    When I press "C-u C-u C-d"
    Then the buffer's contents should be "qrstuvwxyz"

  Scenario: uud does delete 16 times
    When I press "uud"
    Then the buffer's contents should be "qrstuvwxyz"
