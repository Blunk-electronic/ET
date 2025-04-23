------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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


with et_terminals;						use et_terminals;
with et_route_restrict.packages;
with et_via_restrict.packages;
with et_keepout;						use et_keepout;
with et_stopmask;						use et_stopmask;
with et_stopmask.packages;
with et_stencil;						use et_stencil;


with et_text;							use et_text;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;

with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;

with et_assy_doc;						use et_assy_doc;
with et_assy_doc.packages;

with et_symbols;	
with et_schematic;						use et_schematic;
with et_devices_electrical;				use et_devices_electrical;
with et_pcb;							use et_pcb;
with et_pcb_stack;						use et_pcb_stack;
with et_packages;						use et_packages;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
with et_board_shapes_and_text;			use et_board_shapes_and_text;
with et_pcb_contour;					use et_pcb_contour;

with et_nets;

with et_generic_module;					use et_generic_module;
with et_object_status;

package et_device_query_board is

	use pac_polygons;
	use pac_devices_sch;

	use pac_geometry_2;


	
procedure dummy;


	

	

	
end et_device_query_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
