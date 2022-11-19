------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           THERMAL RELIEF                                 --
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


with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
--with ada.containers.indefinite_doubly_linked_lists;
--with ada.containers.ordered_maps;
--with ada.containers.indefinite_ordered_maps;
--with ada.containers.ordered_sets;

--with et_general;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;
--with et_conductor_segment;
with et_terminals;				use et_terminals;

package et_thermal_relief is
	
	use pac_geometry_brd;
	use pac_polygons;
	
	use pac_geometry_2;

	
	-- A fill_zone in a signal layer is usually connected with 
	-- THT or SMD pads (or both) via thermals, solid (or not at all).
	-- For this reason we define a controlled type here because some
	-- properties may exist (or may not exists) depending
	-- on the kind of pad_connection.

	
	keyword_thermal_width : constant string := "thermal_width";		
	keyword_thermal_gap : constant string := "thermal_gap";
	
	thermal_width_min : constant type_track_width := type_track_width'first;
	thermal_width_max : constant type_track_width := 3.0; -- CS: adjust if nessecariy
	
	subtype type_thermal_width is type_distance_positive
		range thermal_width_min .. thermal_width_max;

	
	-- If a terminal is connected/associated with a polyon, then
	-- this is the space between pad and fill_zone:
	thermal_gap_min : constant type_track_clearance := type_track_clearance'first;
	thermal_gap_max : constant type_track_clearance := 3.0; -- CS: adjust if nessecariy
	subtype type_thermal_gap is type_track_clearance range thermal_gap_min .. thermal_gap_max;


	-- Polygons which are connected with a net
	-- can be connected with pads by thermals or solid:
	keyword_pad_connection : constant string := "pad_connection";
	type type_pad_connection is (THERMAL, SOLID);
	pad_connection_default : constant type_pad_connection := THERMAL;

	function to_string (connection : in type_pad_connection) return string;
	function to_pad_connection (connection : in string) return type_pad_connection;

	
	-- Polygons may be connected with SMT, THT or all pad technologies
	-- CS: Is that a reasonable idea ????? it was inherited from kicad.
	keyword_pad_technology : constant string := "pad_technology";
	
	type type_pad_technology is (
		SMT_ONLY,
		THT_ONLY,
		SMT_AND_THT);

	pad_technology_default : constant type_pad_technology := SMT_AND_THT;
	
	function to_string (technology : in type_pad_technology) return string;
	function to_pad_technology (technology : in string) return type_pad_technology;

	
	-- CS not clear how much of this stuff is really important:
	type type_relief_properties is record
		-- whether SMT, THT or both kinds of pads connect with the fill_zone
		technology	: type_pad_technology := pad_technology_default;

		-- The width of the thermal relief spokes.
		-- Applies to ALL pads connected with the zone:
		width		: type_thermal_width := type_thermal_width'first;

		-- the space between pad and fill_zone -- CS: rename to thermal_length ?
		gap			: type_thermal_gap := type_thermal_gap'first;
		-- CS no need ? The gap is the clearance between terminal and zone.
	end record;
	
	
	
	text_thermal_width 	: constant string := "thermal_width";	
	text_thermal_gap 	: constant string := "thermal_gap";	
	text_pad_connection : constant string := "pad_connection";	
	text_pad_technology : constant string := "connected_with";	



	type type_terminal_with_relief is record
		-- The position, face and rotation of the terminal in the board:
		position	: type_terminal_position_fine;

		-- The outline of the terminal in the board:
		outline		: type_polygon;

		-- This cursor points to the terminal as defined in the package model:
		terminal	: pac_terminals.cursor; 
	end record;
	
	package pac_terminals_with_relief is new doubly_linked_lists (type_terminal_with_relief);



	-- The spokes of a thermal symbol. These are straight conductor tracks
	-- that start inside the pad and run outward into the surrounding fill zone.
	-- For rectangular or circular pads they look like spokes of a wheel.
	-- Usually there are up to 4 spokes that start at the geometrical
	-- center of the pad. 
	-- For irregular pad contours the spokes may start at arbitrary user defined points
	-- inside the pad - as specified in the terminal properties (see et_terminals.type_terminal):
	package pac_spokes is new doubly_linked_lists (pac_geometry_brd.type_line);

	-- All spokes of a single thermal relief have the same linewidth:
	type type_relief is record -- CS rename to type_thermal_symbol ?
		width	: type_track_width;
		spokes	: pac_spokes.list;
	end record;

	
	-- Creates a thermal relief for the given single terminal.
	-- The width and length of the generated thermal spokes depends on several things:
	-- - zone clearance 
	-- - zone linewidth
	-- - pad geometry
	-- - pad technology
	-- - CS gap size ?
	function make_relief (
		terminal_cursor	: in pac_terminals_with_relief.cursor;
		zone_clearance	: in type_track_clearance;
		zone_linewidth	: in type_track_width)
		return type_relief;
	
	package pac_reliefes is new doubly_linked_lists (type_relief); -- CS rename to pac_thermal_symbols ?


	-- Creates for all given terminals a list of thermal reliefes.
	-- The width and length of the generated thermal spokes depends on several things:
	-- - zone clearance 
	-- - zone linewidth
	-- - pad geometry
	-- - pad technology
	-- - CS gap size ?
	function make_reliefes (
		terminals		: in pac_terminals_with_relief.list;
		zone_clearance	: in type_track_clearance;
		zone_linewidth	: in type_track_width)
		return pac_reliefes.list;
	
	
end et_thermal_relief;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
