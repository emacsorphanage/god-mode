Feature: Regions
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "Here we go"
    And I bind "C-SPC" to "set-mark-command"
    And I have god-mode on
    And I go to beginning of buffer

  Scenario: mark is preserved (control)
    When I send the key sequence "SPC f"
    Then the region should be "H"

  Scenario: mark is preserved (meta)
    When I send the key sequence "SPC gf"
    Then the region should be "Here"

  Scenario: mark is preserved (control+meta)
    When I send the key sequence "SPC gfGf"
    Then the region should be "Here we"
