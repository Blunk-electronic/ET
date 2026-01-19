------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--               COMMAND PROCESSOR / SCHEMATIC / NETS                       --
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

with et_generic_modules;		use et_generic_modules;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_cmd_sts;				use et_cmd_sts;



package et_cp_schematic_nets is


	-- Generates the netlist files of all assembly variants from the given top module.
	-- The netlist files are named after the module name and the variant name.
	procedure export_netlist (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);
	-- CS should be in a CAM related package ?



	procedure set_net_scope (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

	

	-- This procedure parses a command that highlights a net.
	-- Example: "schematic demo show net RESET_N"
	procedure show_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	
	-- This procedure parses a command that places a net connector.
	-- Example: "schematic demo place net_connector 1 60 80 input"
	procedure place_net_connector (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	-- This procedure parses a command that deletes a net connector:
	procedure delete_net_connector (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	

	-- This procedure parses a command that places a net connector.
	-- Example: "schematic demo place net_label 1 70 80"
	procedure place_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	-- This procedure parses a command that deletes a net label:
	procedure delete_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);
	

	-- This procedure parses a command that moves a net label:
	procedure move_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);
	
	

	-- This procedure parses a command that set the 
	-- class of a net.
	-- Example: "schematic demo set class GND pwr"
	procedure set_net_class (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	

	-- Parses a command like:
	-- "schematic demo draw net RESET_N 1 90 100  100 100"
	-- The command can be shorter than the above example, because
	-- the operator is not required to type everything:
	procedure draw_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

	
	procedure delete_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	procedure rename_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	
	
	procedure delete_net_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);

	
	procedure drag_net_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);



	
	
	procedure delete_net_strand (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level);


	
end et_cp_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
