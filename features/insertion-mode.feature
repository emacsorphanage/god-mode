Feature: Insertion mode
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I am in insertion mode

  Scenario: C-u u inserts u 4 times
    When I press "C-u u"
    Then the buffer's contents should be "uuuu"

  Scenario: C-u C-u u inserts u 16 times
    When I press "C-u C-u u"
    Then the buffer's contents should be "uuuuuuuuuuuuuuuu"
