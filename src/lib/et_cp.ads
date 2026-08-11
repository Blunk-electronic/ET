------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          COMMAND PROCESSOR                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.
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


with et_logging;				use et_logging;
with et_cmd_sts;				use et_cmd_sts;
with et_script_names;			use et_script_names;


package et_cp is


	-- Executes a command like
	-- "schematic motor_driver draw net motor_on 1 150 100 150 130".
	-- Dispatches further to the execution of either schematic,
	-- board or project commands.
	-- When called, the current working directory must be the
	-- project like my_projects/blood_sample_analyzer.
	procedure execute_script_command (
		-- The script file that contains the command. for debug messages only:
		script_name		: in type_script_name;
		-- The text fields like "schematic motor_driver draw net motor_on 1 150 100 150 130":
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level);




end et_cp;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
