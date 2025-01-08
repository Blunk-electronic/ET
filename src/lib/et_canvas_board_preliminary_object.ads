------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS BOARD PRELIMINARY OBJECT                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with et_canvas_board_2;					use et_canvas_board_2;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;

with et_board_layer_category;			use et_board_layer_category;
with et_pcb_stack;						use et_pcb_stack;
with et_net_names;						use et_net_names;
with et_device_name;					use et_device_name;


package et_canvas_board_preliminary_object is

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information here:

	object_layer_category 	: type_layer_category := LAYER_CAT_ASSY;
	
	object_signal_layer		: type_signal_layer := signal_layer_default;

	object_face				: type_face := face_default; -- top, bottom

	object_net_name			: pac_net_name.bounded_string := et_net_names.no_name; -- GND, P3V3
	
	object_device_name		: type_device_name := et_device_name.no_name; -- IC45, FD2
	
	
	-- Resets the preliminary object information
	-- and global stuff like object_tool, object_ready, ...
	-- (see et_canvas spec):
	procedure reset_preliminary_object;
	

	
end et_canvas_board_preliminary_object;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
