------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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
-- 	  - rework procedures so that a module cursor
--		is used instead the module_name.
--    - move stuff related to signal layers to separate child package


with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.exceptions;			use ada.exceptions;

with ada.containers;			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_nets;						use et_nets;
with et_net_names;					use et_net_names;
with et_module_names;				use et_module_names;
with et_module_instance;			use et_module_instance;
with et_primitive_objects;			use et_primitive_objects;
with et_axes;						use et_axes;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_generic_modules;			use et_generic_modules;
with et_module;						use et_module;
with et_module_board;				use et_module_board;
with et_module_board_user_settings;	use et_module_board_user_settings;

with et_text;

with et_assembly_technology;		use et_assembly_technology;
with et_terminal_name;				use et_terminal_name;
with et_terminals;					use et_terminals;

with et_pcb_sides;					use et_pcb_sides;
with et_board_layer_category;		use et_board_layer_category;
with et_board_coordinates;			use et_board_coordinates;
with et_board_geometry;				use et_board_geometry;
use et_board_geometry.pac_geometry_2;

with et_design_rules_board;			use et_design_rules_board;

with et_exceptions;					use et_exceptions;
with et_object_status;				use et_object_status;
with et_logging;					use et_logging;
with et_exceptions;					use et_exceptions;


package et_board_ops is

	use pac_generic_modules;


	procedure dummy;


	
end et_board_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
