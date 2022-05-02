------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       VIA RESTRICT BOARDS                                --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with et_conductor_text.boards;

package et_via_restrict.boards is
	--use pac_geometry_brd;

	use pac_geometry_2;
	use pac_text_fab;


	-- GUI relevant only: The line width of via restrict:
	via_restrict_line_width : constant type_general_line_width := text_parameters_fab.width_min;
	
	type type_via_restrict_line is new 
		et_via_restrict.type_via_restrict_line with
	record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_lines is new doubly_linked_lists (type_via_restrict_line);

	
	type type_via_restrict_arc is new
		et_via_restrict.type_via_restrict_arc with 
	record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_arcs is new doubly_linked_lists (type_via_restrict_arc);

	
	type type_via_restrict_circle is new
		et_via_restrict.type_via_restrict_circle with
	record
		layers	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_circles is new doubly_linked_lists (type_via_restrict_circle);

	
	type type_via_restrict_contour is new
		et_via_restrict.type_via_restrict_contour with
	record
		layers 	: type_signal_layers.set;
	end record;
	
	package pac_via_restrict_contours is new doubly_linked_lists (type_via_restrict_contour);


	type type_via_restrict_cutout is new
		et_via_restrict.type_via_restrict_cutout with
	record
		layers 	: type_signal_layers.set;
	end record;
		
	package pac_via_restrict_cutouts is new doubly_linked_lists (type_via_restrict_cutout);
	
	
	type type_via_restrict is record
		lines 		: pac_via_restrict_lines.list;
		arcs		: pac_via_restrict_arcs.list;
		circles		: pac_via_restrict_circles.list;
		contours	: pac_via_restrict_contours.list;
		cutouts		: pac_via_restrict_cutouts.list;
		texts		: et_conductor_text.boards.pac_conductor_texts.list; -- for notes on routing
	end record;


	-- Logs the properties of the given line of via restrict
	procedure line_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given arc of via restrict
	procedure arc_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- CS procedure circle_via_restrict_properties


	
end et_via_restrict.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
