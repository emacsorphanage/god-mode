Feature: Predicate based disable
  Background:
    Given god-mode is enabled for all buffers
    And I am in buffer "god-mode-test"
    And I describe function "god-mode"
    And I grep current directory
    And I start ielm

  Scenario: God mode is automatically enabled for fundamental-mode
    When I switch to buffer "god-mode-test"
    Then god-mode is enabled

  Scenario: God mode is disabled for help-mode (a special derived mode)
    When I switch to buffer "*Help*"
    Then god-mode is disabled

  Scenario: God mode is disabled for grep-mode (an explicitly disabled mode)
    When I switch to buffer "*grep*"
    Then god-mode is disabled

  Scenario: God mode is disabled in buffers with view-mode
    When I open a view-mode buffer
    Then god-mode is disabled

  Scenario: God mode is disabled in comint derived buffers
    When I switch to buffer "*ielm*"
    Then god-mode is disabled

  Scenario: God mode is disabled in mode-class special buffers
    When I open a test-special-mode buffer
    Then god-mode is disabled
