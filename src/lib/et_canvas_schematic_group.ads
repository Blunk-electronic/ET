------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC NETS                            --
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
-- DESCRIPTION:
-- 

with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with gdk.event;						use gdk.event;
with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.widget;					use gtk.widget;
with gtk.gentry;

with et_canvas_schematic;			use et_canvas_schematic;
with et_net_names;					use et_net_names;
with et_canvas_messages;			use et_canvas_messages;
with et_canvas_tool;				use et_canvas_tool;
with et_sheets;						use et_sheets;
with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
use et_schematic_geometry.pac_geometry_2;
-- use et_schematic_geometry.pac_path_and_bend;

-- with et_text;
with et_generic_modules;			use et_generic_modules;
-- with et_net_labels;					use et_net_labels;
-- with et_nets;						use et_nets;
-- with et_net_segment;				use et_net_segment;
-- with et_net_strands;				use et_net_strands;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

package et_canvas_schematic_group is

	use pac_generic_modules;
	


	-- status_draw_net : constant string := 
	-- 	status_click_left 
	-- 	& "or "
	-- 	& status_press_space
	-- 	& status_set_A;
		-- & status_hint_for_abort;


	
end et_canvas_schematic_group;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
