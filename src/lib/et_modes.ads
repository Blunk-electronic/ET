------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           OPERATING MODES                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


package et_modes is

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:	
	runmode_prefix : constant string := "MODE_";

	type type_runmode is (
		MODE_HEADLESS, -- no GUI. commandline only
		MODE_SYMBOL,
		MODE_PACKAGE,
		MODE_DEVICE,
		MODE_MODULE,
		MODE_RIG
		);

	runmode_default : constant type_runmode := MODE_MODULE;

	runmode : type_runmode := runmode_default;
	
	function to_runmode (mode : in string) return type_runmode;
	function to_string (mode : in type_runmode) return string;


	-- Commands can be entered via:
	-- - the console in the GUI as single command.
	-- - via a script (a batch of commands)
	type type_cmd_entry_mode is (
		SINGLE_CMD,
		VIA_SCRIPT
		);

	cmd_entry_mode_default : constant type_cmd_entry_mode := SINGLE_CMD;

	cmd_entry_mode : type_cmd_entry_mode := cmd_entry_mode_default;
	
	function to_string (entry_mode : in type_cmd_entry_mode) return string;
	

	
	
	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	verb_prefix		: constant string := ("VERB_");
	noun_prefix		: constant string := ("NOUN_");
	domain_prefix	: constant string := ("DOM_");
	
	type type_expect_entry is (
		EXP_VERB,
		EXP_NOUN);

	expect_entry_default : constant type_expect_entry := EXP_VERB;
		

	
end et_modes;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
