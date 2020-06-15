Feature: Prefix help
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I have god-mode on
    And I bind "C-c C-e" to "end-of-line"
    And I bind "M-s C-e" to "end-of-line"

  Scenario: shows help on entering C-h (control)
    When I send the key sequence "C-c C-h"
    Then there is a "*Help*" buffer

  Scenario: help buffer shows relevant bindings (control)
    Given I send the key sequence "C-c C-h"
    When I switch to buffer "*Help*"
    Then the buffer's contents should contain "Major Mode Bindings Starting With C-c:"
    And the buffer's contents should contain "Global Bindings Starting With C-c:"

  Scenario: shows help on entering C-h (meta)
    When I send the key sequence "M-s C-h"
    Then there is a "*Help*" buffer

  Scenario: help buffer shows relevant bindings (meta)
    Given I send the key sequence "M-s C-h"
    When I switch to buffer "*Help*"
    Then the buffer's contents should contain "Major Mode Bindings Starting With M-s:"
    And the buffer's contents should contain "Global Bindings Starting With M-s:"
