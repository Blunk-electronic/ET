------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       ROUTE RESTRICT / BOARDS                            --
--                                                                          --
--                              S p e c                                     --
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

with et_pcb_signal_layers;			use et_pcb_signal_layers;


package et_route_restrict.boards is

	use pac_geometry_2;
	use pac_text_board;


	
-- LINES:

	type type_route_restrict_line is new 
		et_route_restrict.type_route_restrict_line with
	record
		layers 	: pac_signal_layers.set;
	end record;


	-- CS procedure to set linewidth and layer

	procedure reset_line (
		line : in out type_route_restrict_line);

	
	package pac_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	
	
	
	
	
	
-- ARCS:


	type type_route_restrict_arc is new
		et_route_restrict.type_route_restrict_arc with 
	record
		layers 	: pac_signal_layers.set;
	end record;

	
	procedure reset_arc (
		arc : in out type_route_restrict_arc);
	
	
	package pac_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);


	
	
	
-- CIRCLES:
	
	type type_route_restrict_circle is new
		et_route_restrict.type_route_restrict_circle with 
	record
		layers 	: pac_signal_layers.set;
	end record;
	
	
	procedure reset_circle (
		circle : in out type_route_restrict_circle);
	
	
	package pac_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	
	

	
-- ZONE:

	type type_route_restrict_contour is new -- CS rename to type_route_restrict_zone
		et_route_restrict.type_route_restrict_zone with
	record
		layers 	: pac_signal_layers.set;
	end record;

	package pac_route_restrict_contours is new doubly_linked_lists (type_route_restrict_contour);
	-- CS rename to pac_route_restrict_zones
	
	type type_route_restrict_cutout is new
		et_route_restrict.type_route_restrict_cutout with
	record
		layers 	: pac_signal_layers.set;
	end record;
		
	package pac_route_restrict_cutouts is new doubly_linked_lists (type_route_restrict_cutout);

	
	-- NOTE: 
	-- In the board drawing there is no "both-sides" as with
	-- silkscreen or assembly documentation. Here the signal
	-- layers specify which conductor layers are affected.
	
	type type_route_restrict is record
		lines 		: pac_route_restrict_lines.list;
		arcs		: pac_route_restrict_arcs.list;
		circles		: pac_route_restrict_circles.list;
		contours	: pac_route_restrict_contours.list; -- CS rename contours to zone
		cutouts		: pac_route_restrict_cutouts.list;

		-- CS texts : 
		-- This must not be derived from from conductor text because
		-- it is not fabrication relevant.
		-- It should contain notes of the designer exclusively.
	end record;

	


	-- Logs the properties of the given line of route restrict
	-- procedure line_route_restrict_properties (
	-- 	face			: in type_face;
	-- 	cursor			: in pac_route_restrict_lines.cursor;
	-- 	log_threshold 	: in type_log_level);
 -- 
	-- -- Logs the properties of the given arc of route restrict
	-- procedure arc_route_restrict_properties (
	-- 	face			: in type_face;
	-- 	cursor			: in pac_route_restrict_arcs.cursor;
	-- 	log_threshold 	: in type_log_level);

	-- CS procedure circle_route_restrict_properties

	
	
end et_route_restrict.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
