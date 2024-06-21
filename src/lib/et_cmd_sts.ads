------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            COMMAND STATUS                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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
-- <http://www.gnu.org/licenses/>.                                          --
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

with ada.text_io;				use ada.text_io;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;


package et_cmd_sts is

	-- In graphical mode (other than runmode headless) and 
	-- single command entry mode (cmd_entry_mode) a command may be
	-- incomplete. It will then be completed via an interactive
	-- graphical completition process.
	-- If the command is waiting for finalization (like pressing space key
	-- to place a unit or to draw a net) then the flag 
	-- finalization_pending goes true.
	type type_single_cmd_status is record

		-- the command to be executed like "schematic blood_sample_analyzer set value C1 100n"
		cmd			: type_fields_of_line;

		-- Goes false if too few arguments given via console:
		complete	: boolean := true;

		-- Indicates that the command is in progress,
		-- but not finalized yet:
		finalization_pending : boolean := false;
	end record;	


	-- In graphical mode, scripts can be nested.
	-- In script mode we register only the first
	-- exception regardless of the nesting depth.
	-- Because the operator needs to know which script
	-- has actually failed at which line.
	-- The failed script will then be output in the status bar.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	-- For this reason this type is provided:
	type type_script_cmd_status is record
		-- the name of the script file like "rename_power_nets.scr":
		script_name	: pac_script_name.bounded_string;

		-- the command to be executed like "schematic blood_sample_analyzer set value C1 100n"
		cmd			: type_fields_of_line;

		-- the flag that indicates whether the command failed
		failed		: boolean := false;
	end record;
	

	-- The global variable that stores the status of the latest
	-- script command.
	-- IN HEADLESS MODE THIS STUFF HAS NO MEANING !
	script_cmd_status : type_script_cmd_status;
	

	
end et_cmd_sts;

