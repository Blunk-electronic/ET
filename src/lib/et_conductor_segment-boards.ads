------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     CONDUCTOR SEGMENT IN BOARD                           --
--                                                                          --
--                              S p e c                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_pcb_stack;				use et_pcb_stack;
with et_design_rules;			use et_design_rules;
with et_string_processing;		use et_string_processing;


package et_conductor_segment.boards is

	-- In a pcb drawing, objects in conductor layers can be placed 
	-- in various layers.
	-- This requires a layer id for the object.
	type type_conductor_line is new et_conductor_segment.type_conductor_line with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_lines is new doubly_linked_lists (type_conductor_line);

	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_conductor_lines.list;
		process	: not null access procedure (position : in pac_conductor_lines.cursor);
		proceed	: not null access boolean);
	
	
	
	-- Returns true if the given point sits on the given line.
	function on_segment (
		point		: in type_point; -- x/y
		layer		: in type_signal_layer;
		line		: in pac_conductor_lines.cursor)
		return boolean;
	
	type type_conductor_arc is new et_conductor_segment.type_conductor_arc with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_arcs is new doubly_linked_lists (type_conductor_arc);

	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		arcs	: in pac_conductor_arcs.list;
		process	: not null access procedure (position : in pac_conductor_arcs.cursor);
		proceed	: not null access boolean);


	
	-- Returns true if the given point sits on the given arc.
	function on_segment (
		point		: in type_point; -- x/y
		layer		: in type_signal_layer;
		arc			: in pac_conductor_arcs.cursor)
		return boolean;
	
	type type_conductor_circle is new et_conductor_segment.type_conductor_circle with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;

	package pac_conductor_circles is new indefinite_doubly_linked_lists (type_conductor_circle);
	


	
end et_conductor_segment.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
