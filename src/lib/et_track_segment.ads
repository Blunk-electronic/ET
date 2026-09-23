------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           TRACK SEGMENT                                  --
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
--
-- DESCRIPTION:
--
-- This package is about basic objects that are used to make
-- tracks (or traces) where current flows. Basic elements are
-- lines, arcs and circles.
-- A track can be part of a net or it can be so called freetrack.
-- A freetrack is not connected to a net but can carry current nevertheless.
-- A track or a freetrack segment can exist in any signal layer.
--
--
-- history of changes:
--
--
--
-- To Do:
--
--
--

with ada.containers;			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_board_geometry;			use et_board_geometry;
with et_pcb_signal_layers;		use et_pcb_signal_layers;
with et_conductor_segment;


package et_track_segment is

	use pac_geometry_2;


	-- In the board drawing, objects in conductor
	-- layers can be placed in various layers.
	-- This requires a layer id for the object.

	type type_track_line is new et_conductor_segment.type_conductor_line with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;



	-- CS procedure to set linewidth and layer

	-- Resets A, B, width and layer to default:
	procedure reset_line (
		line : in out type_track_line);


	-- Returns the signal layer of the given line:
	function get_layer (
		line : in type_track_line)
		return type_signal_layer;



	-- Returns the start/end point and layer as string.
	-- If "width" is true, then the segment width is also output:
	function to_string (
		line	: in type_track_line;
		width	: in boolean)
		return string;


	-- Returns true if the given line segments are connected.
	-- Criteria for "Connected" are:
	-- 1. Their start/end points sit on top of each
	--    other so that a chain is formed.
	-- 2. One line starts or ends between start and end
	--    of the other line.
	-- By default the signal layer is checked. If the given lines
	-- are in different layers, then they are regarded as not
	-- connected IN ANY CASE.
	-- If "observe_layer" is false, then the layer is ignored. This
	-- option is useful when computing the ratsnest (or airwires):
	function are_connected (
		line_1, line_2	: in type_track_line;
		observe_layer	: in boolean := true)
		return boolean;


	-- Returns true if the given line is in the
	-- given signal layer and in the given area:
	function in_layer_and_in_area (
		line	: in type_track_line;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean;









	type type_track_arc is new et_conductor_segment.type_conductor_arc with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;


	-- Resets A, B, center, width and layer to default:
	procedure reset_arc (
		arc : in out type_track_arc);



	-- Returns the signal layer of the given arc:
	function get_layer (
		arc : in type_track_arc)
		return type_signal_layer;


	-- Returns the start/end point, center and layer as string.
	-- If "width" is true, then the segment width is also output:
	function to_string (
		arc		: in type_track_arc;
		width	: in boolean)
		return string;



	-- Returns true if the given arc is in the
	-- given signal layer and in the given area:
	function in_layer_and_in_area (
		arc		: in type_track_arc;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean;






-- CIRCLES:

	type type_track_circle is new et_conductor_segment.type_conductor_circle with record
		layer	: type_signal_layer := type_signal_layer'first;
	end record;


	-- Returns the signal layer of the given circle:
	function get_layer (
		circle : in type_track_circle)
		return type_signal_layer;


	-- Returns the center, radius and layer as string.
	-- If "width" is true, then the segment width is also output:
	function to_string (
		circle	: in type_track_circle;
		width	: in boolean)
		return string;


	-- Returns true if the given circle is in the
	-- given signal layer and in the given area:
	function in_layer_and_in_area (
		circle	: in type_track_circle;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean;



end et_track_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
