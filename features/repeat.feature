Feature: Repeat
  Background:
    Given I am in buffer "god-mode-test"
    And the buffer is empty
    And I insert "abcdefghijklmnopqrstuvwxyz one two three"
    And I have god-mode on
    And I go to line "1"

  Scenario: M-5 C-d C-x z does delete 5 times then 5 more times
    When I press "M-5 C-d C-x z"
    Then the buffer's contents should be "klmnopqrstuvwxyz one two three"

  Scenario: 5dx z does delete 5 times then 5 more times
    When I press "5dx z"
    Then the buffer's contents should be "klmnopqrstuvwxyz one two three"

  Scenario: M-f C-x z M-d deletes the second word
    When I press "M-f C-x z M-d"
    Then the buffer's contents should be "abcdefghijklmnopqrstuvwxyz one three"

  Scenario: gfx zgd deletes the second word
    When I press "gfx zgd"
    Then the buffer's contents should be "abcdefghijklmnopqrstuvwxyz one three"
