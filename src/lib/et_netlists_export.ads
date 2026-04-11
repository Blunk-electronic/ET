------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         NETLISTS / EXPORT                                --
--                                                                          --
--                              S p e c                                     --
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
--   ToDo: 


with ada.containers;            use ada.containers;

with et_assembly_variant_name;	use et_assembly_variant_name;
with et_logging;				use et_logging;
with et_module_names;			use et_module_names;
with et_netlists;				use et_netlists;


package et_netlists_export is

	
	comment_mark : constant string := "#";
	
	
	-- If write_file ist true, creates the netlist file (which inevitably and intentionally 
	-- overwrites the previous file).
	-- - modules contains the modules and their nets ordered in a tree structure.
	-- - module_name is the name of the top module. to be written in the header of the netlist file.
	-- - The netlist file will be named after the module name and assembly variant.	
	-- - Exports the netlist of the given module to the export/CAM directory.
	function make_netlist (
		modules			: in pac_netlist_modules.tree;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		write_file		: in boolean;
		log_threshold	: in type_log_level)
		return pac_module_netlist.tree;


	
end et_netlists_export;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
