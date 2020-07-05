Feature: Regions
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "Here we go"
    And I have god-mode on
    And I go to beginning of buffer
    And I set the mark

  Scenario: mark is preserved (control)
    When I send the key sequence "f"
    Then the region should be "H"

  Scenario: mark is preserved (meta)
    When I send the key sequence "gf"
    Then the region should be "Here"

  Scenario: mark is preserved (control+meta)
    When I send the key sequence "gfGf"
    Then the region should be "Here we"
