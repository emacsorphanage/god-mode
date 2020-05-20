Feature: Execute with current bindings
  Background:
    Given I am in buffer "god-mode-test"
    And I bind "C-e" to "god-execute-with-current-bindings"
    And the buffer is empty
    And I insert "abcdefghijkl"
    And I go to beginning of buffer

  Scenario: no effect when already in God mode
    Given I have god-mode on
    When I send the key sequence "C-e q Z"
    Then the buffer's contents should be "Zabcdefghijkl"
    And god-mode is enabled
    
  Scenario: runs one command in God mode
    Given I am in insertion mode
    When I send the key sequence "C-e q Z"
    Then the buffer's contents should be "Zabcdefghijkl"
    And god-mode is disabled

  Scenario: handles prefix arguments
    Given I am in insertion mode
    When I send the key sequence "C-u 1 C-e 0 f"
    Then the cursor should be at point "11"
