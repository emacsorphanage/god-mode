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
    And I send the key sequence "kk"
    Then the buffer's contents should be:
    """
    impatience
    hubris
    """

  Scenario: map / into undo
    Given I go to line "1"
    And I send the key sequence "kk"
    And I send the key sequence "//"
    Then the buffer's contents should be:
    """
    laziness
    impatience
    hubris
    """

  Scenario: execute named keyboard macro
    Given I go to line "1"
    And I bind a named keyboard macro which kills line to C-c C-r
    And I send the key sequence "cr"
    Then the buffer's contents should be:
    """
    impatience
    hubris
    """
