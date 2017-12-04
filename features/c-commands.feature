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

  Scenario: execute C-[god-literal-key]
    Given I bind "C-SPC" to "set-mark-command"
    And I go to beginning of buffer
    And I send the key sequence "SPC"
    And I go to end of buffer
    And I send the key sequence "u SPC"
    Then the cursor should be at point "1"

  Scenario: execute commands with C-arrow
    Given I bind "C-x C-<left>" to "backward-word"
    And I go to line "1"
    And I go to end of line
    And I send the key sequence "x <left>"
    Then the cursor should be at point "1"

  Scenario: execute commands with uppercase letters (control)
    Given I bind "C-S-A" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "A"
    Then the cursor should be at point "1"

  Scenario: execute commands with uppercase letters (met)
    Given I bind "M-S-A" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "g A"
    Then the cursor should be at point "1"

  Scenario: execute commands with uppercase letters (combination)
    Given I bind "C-x C-S-A" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "xA"
    Then the cursor should be at point "1"

  Scenario: execute commands with shifted arrows (control)
    Given I bind "C-x C-S-<left>" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "x S-<left>"
    Then the cursor should be at point "1"

  Scenario: execute commands with shifted arrows (meta)
    Given I bind "M-S-<left>" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "g S-<left>"
    Then the cursor should be at point "1"

  Scenario: execute commands with shifted arrows (space)
    Given I bind "C-c S-<left>" to "beginning-of-buffer"
    And I go to end of buffer
    And I send the key sequence "C-c S-<left>"
    Then the cursor should be at point "1"
