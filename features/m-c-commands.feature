Feature: M-C- commands
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "Here we go"
    Then I have god-mode on

  Scenario: mark is preserved
    Given I go to beginning of buffer
    And I bind "C-SPC" to "set-mark-command"
    When I send the key sequence "SPC gfGf"
    Then region's contents should be "Here we"



