Feature: Digit arguments
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "abcdefghijklmnopqrstuvwxyz"
    And I have god-mode on
    And I go to line "1"

  Scenario: M-5 C-d does delete 5 times
    When I press "M-5 C-d"
    Then the buffer's contents should be "fghijklmnopqrstuvwxyz"

  Scenario: 5d does delete 5 times
    When I press "5d"
    Then the buffer's contents should be "fghijklmnopqrstuvwxyz"
