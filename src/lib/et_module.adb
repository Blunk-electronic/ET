------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               MODULE                                     --
--                                                                          --
--                               B o d y                                    --
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
--  ToDo:
--


-- with et_exceptions;				use et_exceptions;


-- with ada.text_io;			use ada.text_io;
package body et_module is




	function get_preferred_device_libraries_schematic (
		module : in type_generic_module)
		return pac_library_paths_schematic.list
	is begin
		return get_device_libraries (module.meta.schematic);
	end get_preferred_device_libraries_schematic;


	function get_preferred_device_libraries_board (
		module : in type_generic_module)
		return pac_library_paths_board.list
	is begin
		return get_device_libraries (module.meta.board);
	end get_preferred_device_libraries_board;





	function get_design_rules (
		module : in type_generic_module)
		return type_design_rules
	is begin
		return module.rules;
	end get_design_rules;




	function design_rules_schematic_assigned (
		module : in type_generic_module)
		return boolean
	is begin
		return schematic_rules_assigned (module.rules);
	end design_rules_schematic_assigned;


	function design_rules_board_assigned (
		module : in type_generic_module)
		return boolean
	is begin
		return board_rules_assigned (module.rules);
	end design_rules_board_assigned;






	function get_grid_schematic (
		module : in type_generic_module)
		return et_schematic_geometry.pac_grid.type_grid
	is begin
		return module.grid;
	end get_grid_schematic;



	function get_grid_board (
		module : in type_generic_module)
		return et_board_geometry.pac_grid.type_grid
	is begin
		return module.board.grid;
	end get_grid_board;




	function variant_exists (
		module	: in type_generic_module;
		variant	: in pac_assembly_variant_name.bounded_string)
		return boolean
	is begin
		return variant_exists (module.assembly_variants, variant);
	end variant_exists;



	function get_active_variant (
		module	: in type_generic_module)
		return pac_assembly_variant_name.bounded_string
	is begin
		return module.assembly_variants.active;
	end get_active_variant;



	function get_variant_count (
		module	: in type_generic_module)
		return natural
	is begin
		return get_count (module.assembly_variants);
	end get_variant_count;

end et_module;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
