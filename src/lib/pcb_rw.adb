------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                PCB_RW                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	--use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;
with ada.tags;

with ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;				use et_general;

with et_pcb_coordinates;		use et_pcb_coordinates;
use et_pcb_coordinates.geometry;

with et_string_processing;
with et_packages;
with et_pcb;
with et_pcb_stack;
with general_rw;				use general_rw;
with et_geometry;				use et_geometry;
with et_text;					use et_text;
with et_packages;

package body pcb_rw is

	procedure write_text_properties (t : in et_packages.type_text'class) is
		use et_packages;
	begin
-- 		write (keyword => keyword_position, parameters => position (text.position) & 
-- 			space & keyword_rotation & to_string (get_angle (text.position))
-- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		write (keyword => keyword_position, parameters => position (t.position));
			-- position x 0.000 y 5.555 rotation 0.00
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
			keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			keyword_vertical   & space & to_string (t.alignment.vertical));
		
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties;

	procedure write_text_properties_with_face (
		t		: in et_packages.type_text'class;
		face	: in et_pcb_coordinates.type_face) 
		is
		use et_packages;
	begin
		write (keyword => keyword_position, parameters => position (t.position) & 
			space & keyword_face & to_string (face)); -- position x 0.000 y 5.555 rotation 0.00 face top

		-- CS this could be more elegant way. did not get it working
		-- 		write (keyword => keyword_position, parameters => 
		-- 			   position (type_position (text.position with face => face))
		-- 			  );
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
				);
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties_with_face;

	procedure write_text (cursor : in et_packages.type_texts_with_content.cursor) is
		use et_packages.type_texts_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, wrap => true,
			parameters => to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;
	
	procedure write_width (width : in et_packages.type_track_width) is begin
		write (keyword => keyword_width, parameters => to_string (width));
	end;

	procedure write_line (line : in et_packages.pac_shapes.type_line'class) is
	-- writes start and end point of a line
		use et_packages.pac_shapes;
	begin
		write (keyword => keyword_start, parameters => position (line.start_point));
		write (keyword => keyword_end  , parameters => position (line.end_point));
	end write_line;

	procedure write_arc (arc : in et_packages.pac_shapes.type_arc'class) is 
	-- writes center, start and end point of an arc
		use et_packages.pac_shapes;
	begin
		write (keyword => keyword_center, parameters => position (arc.center));
		write (keyword => keyword_start, parameters => position (arc.start_point));
		write (keyword => keyword_end, parameters => position (arc.end_point));
		write (keyword => et_geometry.keyword_direction, parameters => to_string (arc.direction));		
	end write_arc;

	procedure write_circle (circle : in et_packages.pac_shapes.type_circle'class) is 
	-- writes center and radius of a circle
		use et_packages.pac_shapes;
	begin
		write (keyword => keyword_center, parameters => position (circle.center));
		write (keyword => keyword_radius, parameters => to_string (circle.radius));
	end write_circle;

	
	procedure write_hatching (hatching : in et_packages.type_hatching) is
		use et_packages;
	begin
		write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (hatching.spacing));
		write (keyword => keyword_hatching_border_width, parameters => to_string (hatching.border_width));
	end;

	procedure write_hatching (hatching : in et_packages.type_hatching_copper) is
		use et_packages;
	begin
		write (keyword => keyword_hatching_line_width  , parameters => to_string (hatching.line_width));
		write (keyword => keyword_hatching_line_spacing, parameters => to_string (hatching.spacing));
		write (keyword => keyword_hatching_border_width, parameters => to_string (hatching.border_width));
	end;
	
	procedure write_easing (easing: in et_packages.type_easing) is
		use et_packages;
	begin
		write (keyword => keyword_corner_easing, parameters => to_string (easing.style));
		write (keyword => keyword_easing_radius, parameters => to_string (easing.radius));
	end;

	procedure write_thermal (thermal : in et_pcb.type_thermal) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (thermal.technology));
		write (keyword => keyword_thermal_width , parameters => to_string (thermal.width));
		write (keyword => keyword_thermal_gap   , parameters => to_string (thermal.gap));	
	end;

	procedure write_width_min (width : in et_packages.type_track_width) is 
		use et_packages;
	begin
		write (keyword => keyword_min_width, parameters => to_string (width));
	end;

	procedure write_isolation (iso : in et_packages.type_track_clearance) is 
		use et_packages;
	begin
		write (keyword => keyword_isolation, parameters => to_string (iso));
	end;

	procedure write_priority (prio : in et_pcb.type_polygon_priority) is
		use et_pcb;
	begin
		write (keyword => keyword_priority , parameters => to_string (prio));
	end;

	procedure write_signal_layer (layer : in et_pcb_stack.type_signal_layer) is 
		use et_pcb_stack;
	begin
		write (keyword => keyword_layer, parameters => to_string (layer));
	end;

	procedure write_fill_stlye (fill_style : in et_packages.type_fill_style) is
		use et_packages;
	begin
		write (keyword => keyword_fill_style, parameters => to_string (fill_style));
	end;

	procedure write_fill_status (filled : in type_filled) is begin
		write (keyword => keyword_filled, parameters => to_string (filled));
	end;
	
	procedure write_pad_connection (connection : in et_pcb.type_polygon_pad_connection) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_connection, parameters => to_string (connection));
	end;

	procedure write_pad_technology (techno : in et_pcb.type_polygon_pad_technology) is
		use et_pcb;
	begin
		write (keyword => keyword_pad_technology, parameters => to_string (techno));
	end;	

	procedure write_signal_layers (layers : in et_pcb_stack.type_signal_layers.set) is
		use et_pcb_stack;
	begin
		write (keyword => keyword_layers, parameters => to_string (layers));
	end;
	
	procedure write_circle_fillable (circle : in et_packages.type_fillable_circle) is 
		use et_packages;
		use et_packages.pac_shapes;
	begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_fillable;

	-- CS unify the follwing two procedures write_circle_copper:
	procedure write_circle_copper (circle : in et_packages.type_copper_circle) is 
	-- Writes the properties of a circle in copper as used in a package.
		use et_packages;
		use et_packages.pac_shapes;
	begin
		circle_begin;
		write_circle (circle);
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_copper;

	procedure write_circle_copper (circle : in et_pcb.type_copper_circle) is 
	-- Writes the properties of a circle in copper as used in a freetrack.		
		use et_packages;
		use et_packages.pac_shapes;
	begin
		circle_begin;
		write_circle (circle);
		write_signal_layer (circle.layer);

		-- the signal layer:
		write (keyword => keyword_filled, parameters => space & to_string (circle.filled));
		
		case circle.filled is
			when NO =>
				write (keyword => keyword_width, parameters => to_string (circle.border_width));
				
			when YES =>
				write (keyword => keyword_fill_style, parameters => space & to_string (circle.fill_style));

				case circle.fill_style is
					when SOLID => null;
					when HATCHED =>
						write (keyword => keyword_hatching_line_width  , parameters => to_string (circle.hatching.line_width));
						write (keyword => keyword_hatching_line_spacing, parameters => to_string (circle.hatching.spacing));
				end case;

		end case;
		circle_end;
	end write_circle_copper;

	
	procedure write_polygon_segments (polygon : in et_packages.pac_shapes.type_polygon_base) is
	-- writes the segments of a polygon (lines, arcs and circles)
		use et_packages;
		use pac_shapes.pac_polygon_lines;
		use pac_shapes.pac_polygon_arcs;
		use pac_shapes.pac_polygon_circles;		
		
		procedure write_line (cursor : in pac_shapes.pac_polygon_lines.cursor) is begin
			line_begin;
			write_line (element (cursor));
			line_end;
		end;

		procedure write_arc (cursor : in pac_shapes.pac_polygon_arcs.cursor) is begin
			arc_begin;
			write_arc (element (cursor));
			arc_end;
		end;

		procedure write_circle (cursor : in pac_shapes.pac_polygon_circles.cursor) is begin
			circle_begin;
			write_circle (element (cursor));
			circle_end;
		end;

	begin
		iterate (polygon.segments.lines, write_line'access);
		iterate (polygon.segments.arcs, write_arc'access);
		iterate (polygon.segments.circles, write_circle'access);		
	end write_polygon_segments;
	


	
	function to_position ( -- CS combine with next function to_position using the tag test ?
	-- Returns a type_point_2d in the the layout.
		line : in et_string_processing.type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in positive)
		return et_pcb_coordinates.geometry.type_point is
		
		use et_string_processing;

		point : et_pcb_coordinates.geometry.type_point; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				set (
					point	=> point,
					axis	=> X,
					value 	=> to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				set (
					point	=> point,
					axis 	=> Y,
					value 	=> to_distance (f (line, place + 1)));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;
		
	function to_position (
	-- Returns a type_position in the layout.
		line : in et_string_processing.type_fields_of_line; -- "x 23 y 0.2 rotation 90.0"
		from : in positive)
		return et_pcb_coordinates.geometry.type_position is

		use et_string_processing;
		
		point : et_pcb_coordinates.geometry.type_position; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				set (point => point, axis => X, value => to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				set (point => point, axis => Y, value => to_distance (f (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif f (line, place) = keyword_rotation then
				set (point, to_rotation (f (line, place + 1)));
				
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	function position (point : et_pcb_coordinates.geometry.type_point'class) return string is
		use ada.tags;

		xy : constant string := space & keyword_x & to_string (x (point)) 
				& space & keyword_y & to_string (y (point));
	begin
		if point'tag = et_pcb_coordinates.geometry.type_point'tag then
			return xy;
			-- x 162.560 y 98.240
			
		elsif point'tag = et_pcb_coordinates.geometry.type_position'tag then
			return xy 
				& space & keyword_rotation & to_string (rot (et_pcb_coordinates.geometry.type_position (point)));
				-- x 162.560 y 98.240 rotation 180.00
			
		elsif point'tag = type_package_position'tag then
			return xy
				& space & keyword_rotation & to_string (rot (et_pcb_coordinates.geometry.type_position (point)))
				& space & keyword_face & to_string (get_face (type_package_position (point)));
				-- x 162.560 y 98.240 rotation 180.00 face top
		else
			return xy;
		end if;

	end position;
	
	function to_grid (
		line : in et_string_processing.type_fields_of_line; -- "default x 1 y 1"
		from : in positive)
		return et_pcb_coordinates.geometry.type_grid is
		use et_string_processing;
		
		grid : et_pcb_coordinates.geometry.type_grid; -- to be returned

		place : positive := from; -- the field being read from given line

	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				grid.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				grid.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
					
			place := place + 2;
		end loop;
		
		return grid;
	end to_grid;

	function to_layers (
	-- Converts a line like "layers 1 4 17" to a set of signal layers.
	-- Issues warning if a layer number occurs more than once.
		line : in et_string_processing.type_fields_of_line) -- layers 1 3 17
		return et_pcb_stack.type_signal_layers.set is

		use et_pcb;
		use et_pcb_stack;
		use et_pcb_stack.type_signal_layers;
		use et_string_processing;

		layers 		: type_signal_layers.set; -- to be returned
		cursor 		: type_signal_layers.cursor;
		inserted	: boolean;
		layer 		: type_signal_layer;
		place 		: positive := 2; -- we start reading the layer numbers with field 2
	begin
		while place <= positive (field_count (line)) loop

			-- get the layer number from current place
			layer := to_signal_layer (f (line, place));

			-- insert the layer number in the container "layers"
			insert (
				container	=> layers,
				new_item	=> layer,
				inserted	=> inserted,
				position	=> cursor);

			-- warn if layer already in container
			if not inserted then
				log (WARNING, affected_line (line) & "signal layer" & to_string (layer) 
					& " specified multiple times !");
			end if;
			
			place := place + 1; -- advance to next place
		end loop;
		
		return layers;
	end to_layers;

	
	
-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS

	procedure add_polygon_line (line : in out type_board_line) is
		use et_packages.pac_shapes;
		use et_packages.pac_shapes.pac_polygon_lines;

		-- make a polygon line:
		l : type_polygon_line := (et_packages.pac_shapes.type_line (line) with others => <>);
	begin
		-- For each segment of the polygon we add 1 to the total number of polygon segments:
		increment_segment_count;
		
		-- Collect the polygon line. 

		-- Use the current total of segments as id for the current segment:
		l.id := segment_count;
		
		append (polygon.segments.lines, l);

		board_reset_line;
	end;
	
	procedure board_reset_line is begin board_line := (others => <>); end;

	
	procedure add_polygon_arc (arc : in out type_board_arc) is
		use et_packages.pac_shapes;
		use et_packages.pac_shapes.pac_polygon_arcs;

		-- make a polygon arc:
		a : type_polygon_arc := (et_packages.pac_shapes.type_arc (arc) with others => <>);
	begin
		-- For each segment of the polygon we add 1 to the total number of polygon segments:
		increment_segment_count;

		-- collect the polygon arc 

		-- Use the current total of segments as id for the current segment:
		a.id := segment_count;

		append (polygon.segments.arcs, a);

		board_reset_arc;
	end;
	
	procedure board_reset_arc is begin board_arc := (others => <>); end;


	procedure add_polygon_circle (circle : in out type_board_circle) is
		use et_packages.pac_shapes;
		use et_packages.pac_shapes.pac_polygon_circles;

		-- make a polygon circle:
		c : type_polygon_circle := (et_packages.pac_shapes.type_circle (circle) with others => <>);
	begin
		-- For each segment of the polygon we add 1 to the total number of polygon segments:
		increment_segment_count;

		-- collect the polygon circle 

		-- Use the current total of segments as id for the current segment:
		c.id := segment_count;
		
		append (polygon.segments.circles, c);

		board_reset_circle;
	end;

	procedure board_reset_circle is begin board_circle := (others => <>); end;


	procedure check_outline (polygon : in type_polygon) is
		use et_string_processing;
		use et_packages.pac_shapes;
		status : constant type_polygon_status := is_closed (polygon);
	begin
		if status.closed then
			null;
		else
			log (WARNING, "Polygon not properly closed at:");

-- 			iterate (status.gaps, query_point
			-- CS: list points
		end if;
	end check_outline;
	
	procedure read_board_line (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_line. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_line (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_line. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_line.start_point := to_position (line, 2);
			return true;
			
		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_line.end_point := to_position (line, 2);
			return true;
		else
			return false;
		end if;
	end;

	
	procedure read_board_arc (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_arc. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

		elsif kw = et_geometry.keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			board_arc.direction := to_direction (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_arc (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		if kw = keyword_start then -- start x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the start position starting at field 2 of line
			board_arc.start_point := to_position (line, 2);

			return true;

		elsif kw = keyword_end then -- end x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the end position starting at field 2 of line
			board_arc.end_point := to_position (line, 2);

			return true;
			
		elsif kw = keyword_center then -- center x 22.3 y 23.3
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_arc.center := to_position (line, 2);

			return true;

		elsif kw = et_geometry.keyword_direction then -- direction ccw
			expect_field_count (line, 2);

			board_arc.direction := to_direction (f (line, 2));

			return true;
			
		else
			return false;
		end if;
	end;

	
	procedure read_board_circle (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_circle. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := et_pcb_coordinates.geometry.to_distance (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_circle (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_circle. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.pac_shapes;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_center then -- center x 150 y 45
			expect_field_count (line, 5);

			-- extract the center position starting at field 2 of line
			board_circle.center := to_position (line, 2);

			return true;
			
		elsif kw = keyword_radius then -- radius 22
			expect_field_count (line, 2);
			
			board_circle.radius := et_pcb_coordinates.geometry.to_distance (f (line, 2));

			return true;
			
		else
			return false;
		end if;
	end;

	
	procedure increment_segment_count is 
		use et_packages.pac_shapes;
	begin
		polygon.segments_total := polygon.segments_total + 1;
	end increment_segment_count;

	function segment_count return et_packages.pac_shapes.type_polygon_segment_count is begin
		return polygon.segments_total;
	end segment_count;

	
	
	procedure board_reset_signal_layer is 
		use et_pcb_stack;
	begin
		signal_layer := type_signal_layer'first;
	end;

	procedure board_reset_lock_status is
		use et_pcb;
	begin
		board_lock_status := NO;
	end;

	procedure board_reset_line_width is 
		use et_packages;
	begin 
		board_line_width := type_general_line_width'first; 
	end;

	-- package and board relevant:	
	procedure board_reset_circle_fillable is 
		use et_packages;
		use et_packages.pac_shapes;
	begin 
		board_circle		:= (others => <>);
		board_filled		:= type_filled'first;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		
		board_reset_line_width;
	end;

	function to_fillable_circle (
	-- Composes a fillable circle from the given parameters. 
	-- Filled and fill_style are discriminants. Depending on them some parameters
	-- matter or not. See spec for type_fillable_circle.
		circle				: in et_packages.pac_shapes.type_circle;
		filled				: in type_filled;
		fill_style			: in et_packages.type_fill_style;
		circumfence_width	: in et_packages.type_general_line_width;
		hatching			: in et_packages.type_hatching)
		return et_packages.type_fillable_circle is

		use et_packages;
		use et_packages.pac_shapes;

	begin -- to_fillable_circle
		case filled is
			when NO =>
				return (circle with
					filled			=> NO,
					fill_style		=> fill_style,
					border_width	=> circumfence_width);
				
			when YES =>
				case fill_style is
					when SOLID =>
						return (circle with
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (circle with
							filled				=> YES,
							fill_style			=> HATCHED,
							hatching			=> hatching);

				end case;
		end case;
	end to_fillable_circle;
	
	function board_make_fillable_circle return et_packages.type_fillable_circle is 
		use et_packages;
	begin
		return to_fillable_circle (
			circle 				=> pac_shapes.type_circle (board_circle),
			filled				=> board_filled,
			fill_style			=> board_fill_style,
			circumfence_width	=> board_line_width,
			hatching			=> board_hatching);
	end;

	function board_make_fillable_circle_solid return et_packages.type_fillable_circle_solid is 
		use et_packages;
	begin
		return (et_packages.pac_shapes.type_circle (board_circle) with board_filled);
	end;

	function board_make_copper_circle return et_packages.type_copper_circle is
		use et_packages;
		use et_packages.pac_shapes;
	begin
		case board_filled is
			when NO =>
				return (pac_shapes.type_circle (board_circle) with 
					filled			=> NO,
					fill_style		=> SOLID, -- don't care here
					border_width	=> board_line_width);

			when YES =>
				case board_fill_style is
					when SOLID =>
						return (pac_shapes.type_circle (board_circle) with 
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (pac_shapes.type_circle (board_circle) with
							filled		=> YES,
							fill_style	=> HATCHED,
							hatching 	=> board_hatching_copper);
				end case;
		end case;
	end;
	
	procedure board_reset_polygon is
	-- This procdure resets polygon properties to their defaults.
	-- This procdure is used by both package and board parsing procedures read_package and read_module_file.
	-- Some properties have no meaning in packages as remarked below.
		use et_packages;
		use et_packages.pac_shapes;
		use et_pcb_stack;
	begin
		polygon				:= (others => <>);

		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		polygon_pad_connection	:= et_pcb.type_polygon_pad_connection'first; -- board relevant only
		polygon_priority		:= et_pcb.type_polygon_priority'first;  -- board relevant only
		polygon_isolation		:= et_packages.type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only

		thermal					:= (others => <>); -- board relevant only
	end;

	procedure fill_zone_begin is begin section_mark (section_fill_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_fill_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;

-- SILK SCREEN
	procedure write_line (cursor : in et_packages.type_silk_lines.cursor) is 
		use et_packages;
		use type_silk_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_silk_arcs.cursor) is 
		use et_packages;
		use type_silk_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_silk_circles.cursor) is 
		use et_packages;
		use type_silk_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.pac_silk_polygons.cursor) is 
		use et_packages;
		use pac_silk_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;

	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_silk_cutouts.cursor) is 
		use et_packages;
		use pac_silk_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;

		cutout_zone_end;
	end;

-- ASSEMBLY DOCUMENTATION
	procedure write_line (cursor : in et_packages.type_doc_lines.cursor) is 
		use et_packages;
		use type_doc_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_doc_arcs.cursor) is 
		use et_packages;
		use type_doc_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_doc_circles.cursor) is
		use et_packages;
		use type_doc_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.pac_doc_polygons.cursor) is 
		use et_packages;
		use pac_doc_polygons;
	begin
		fill_zone_begin;

		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);

		case element (cursor).fill_style is
			when HATCHED =>
				write_hatching (element (cursor).hatching);

			when others => null;
		end case;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_doc_cutouts.cursor) is 
		use et_packages;
		use pac_doc_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;
	
-- KEEPOUT
	procedure write_line (cursor : in et_packages.type_keepout_lines.cursor) is
		use et_packages.type_keepout_lines;
	begin
		line_begin;
		write_line (element (cursor));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_keepout_arcs.cursor) is 
		use et_packages.type_keepout_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		arc_end;
	end write_arc;
	
	procedure write_circle (cursor : in et_packages.type_keepout_circles.cursor) is 
		use et_packages;
		use et_packages.pac_shapes;
		use type_keepout_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_filled, parameters => to_string (element (cursor).filled));
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_keepout_polygons.cursor) is 
		use et_packages;
		use type_keepout_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;

		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_keepout_cutouts.cursor) is 
		use et_packages;
		use pac_keepout_cutouts;
	begin
		cutout_zone_begin;
		
		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- STOP MASK
	procedure write_line (cursor : in et_packages.type_stop_lines.cursor) is 
		use et_packages;
		use type_stop_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_stop_arcs.cursor) is 
		use et_packages;
		use type_stop_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_stop_circles.cursor) is 
		use et_packages;
		use type_stop_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_stop_polygons.cursor) is 
		use et_packages;
		use type_stop_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);
		
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		contours_begin;		
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_begin;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_stop_cutouts.cursor) is 
		use et_packages;
		use pac_stop_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- STENCIL (OR SOLDER PASTE MASK)
	procedure write_line (cursor : in et_packages.type_stencil_lines.cursor) is 
		use et_packages;
		use type_stencil_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_stencil_arcs.cursor) is 
		use et_packages;
		use type_stencil_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_width, parameters => to_string (element (cursor).width));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_stencil_circles.cursor) is 
		use et_packages;
		use type_stencil_circles;
	begin
		write_circle_fillable (element (cursor));
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_stencil_polygons.cursor) is 
		use et_packages;
		use type_stencil_polygons;
	begin
		fill_zone_begin;
		write_easing (element (cursor).easing);
		write_fill_stlye (element (cursor).fill_style);
					  
		if element (cursor).fill_style = HATCHED then
			write_hatching (element (cursor).hatching);
		end if;

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_stencil_cutouts.cursor) is 
		use et_packages;
		use pac_stencil_cutouts;
	begin
		cutout_zone_begin;
		write_easing (element (cursor).easing);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- ROUTE RESTRICT
	procedure write_line (cursor : in et_packages.type_route_restrict_lines.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_route_restrict_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_route_restrict_arcs.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_route_restrict_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));		
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_route_restrict_circles.cursor) is 
		use et_packages;
		use type_route_restrict_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_route_restrict_polygons.cursor) is 
		use et_packages;
		use type_route_restrict_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_route_restrict_cutouts.cursor) is 
		use et_packages;
		use pac_route_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- VIA RESTRICT
	procedure write_line (cursor : in et_packages.type_via_restrict_lines.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_via_restrict_arcs.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_via_restrict_circles.cursor) is 
		use et_packages;
		use et_packages.pac_shapes;		
		use et_pcb_stack;		
		use type_via_restrict_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;
	
	procedure write_polygon (cursor : in et_packages.type_via_restrict_polygons.cursor) is 
		use et_packages;
		use et_pcb_stack;
		use type_via_restrict_polygons;
	begin
		fill_zone_begin;
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);			

		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	procedure write_cutout (cursor : in et_packages.pac_via_restrict_cutouts.cursor) is 
		use et_packages;
		use pac_via_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		
		contours_begin;
		write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
		contours_end;
		
		cutout_zone_end;
	end;

-- BOARD CONTOUR
	procedure write_line (cursor : in et_packages.type_pcb_contour_lines.cursor) is 
		use et_packages;
		use type_pcb_contour_lines;
	begin
		line_begin;
		write_line (element (cursor));
		line_end;
	end write_line;

	procedure write_arc (cursor : in et_packages.type_pcb_contour_arcs.cursor) is 
		use et_packages;
		use type_pcb_contour_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_packages.type_pcb_contour_circles.cursor) is 
		use et_packages;
		use type_pcb_contour_circles;
		use et_pcb_coordinates.geometry;		
	begin
		circle_begin;
		write_circle (element (cursor));
		circle_end;
	end write_circle;
	
	procedure write_line (cursor : in et_pcb.type_pcb_contour_lines.cursor) is 
		use et_pcb;
		use type_pcb_contour_lines;
	begin
		line_begin;
		write_line (element (cursor));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		line_end;
	end write_line;
	
	procedure write_arc (cursor : in et_pcb.type_pcb_contour_arcs.cursor) is 
		use et_pcb;
		use type_pcb_contour_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		arc_end;
	end write_arc;

	procedure write_circle (cursor : in et_pcb.type_pcb_contour_circles.cursor) is 
		use et_pcb;
		use type_pcb_contour_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write (keyword => keyword_locked, parameters => to_string (element (cursor).locked));
		circle_end;
	end write_circle;


	procedure create_package (
	-- Creates a package and stores the package in container et_packages.packages.								 								 
		package_name 	: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		appearance		: in et_packages.type_package_appearance;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_packages;
	begin
		log (text => "creating package " & to_string (package_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if package already exists. If already exists, issue warning and exit.
		if type_packages.contains (packages, package_name) then
			log (WARNING, text => "package already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when REAL =>
					type_packages.insert (
						container	=> packages,
						key			=> package_name,
						new_item	=> (appearance => REAL, others => <>)
						);

				when VIRTUAL =>
					type_packages.insert (
						container	=> packages,
						key			=> package_name,
						new_item	=> (appearance => VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_package;
	

	procedure save_package (
	-- Saves the given package model in a file specified by file_name.
		file_name 		: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac							   
		packge			: in et_packages.type_package; -- the actual package model
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_packages;
		use type_package_model_file;
		
		file_handle : ada.text_io.file_type;
		
		use type_texts_with_content;
		
		procedure write_copper is
		-- This is about copper objects in either top or bottom.
		-- These objects have no connection to any pad or signal.

			use type_copper_lines;
			procedure write_line (cursor : in type_copper_lines.cursor) is begin
				line_begin;
				write_line (element (cursor));
				write_width (element (cursor).width);
				line_end;
			end write_line;

			use type_copper_arcs;
			procedure write_arc (cursor : in type_copper_arcs.cursor) is begin
				arc_begin;
				write_arc (element (cursor));
				write_width (element (cursor).width);
				arc_end;
			end write_arc;

			use et_packages.pac_copper_circles;
			procedure write_circle (cursor : in et_packages.pac_copper_circles.cursor) is begin
				write_circle_copper (element (cursor));
			end write_circle;

			use pac_copper_polygons_solid;
			procedure write_polygon (cursor : in pac_copper_polygons_solid.cursor) is begin
				fill_zone_begin;
				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_fill_stlye (element (cursor).fill_style);
				
				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
				contours_end;
				
				fill_zone_end;
			end write_polygon;

			use pac_copper_polygons_hatched;
			procedure write_polygon (cursor : in pac_copper_polygons_hatched.cursor) is begin
				fill_zone_begin;
				write_easing (element (cursor).easing);

				write_width_min (element (cursor).width_min);
				write_isolation (element (cursor).isolation);

				write_fill_stlye (element (cursor).fill_style);
				write_hatching (element (cursor).hatching);

				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
				contours_end;
				
				fill_zone_end;
			end write_polygon;

			use pac_copper_cutouts;
			procedure write_cutout (cursor : in pac_copper_cutouts.cursor) is begin
				cutout_zone_begin;
				write_easing (element (cursor).easing);

				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
				contours_end;
				
				cutout_zone_end;
			end;
			
		begin -- write_copper
			section_mark (section_copper, HEADER);

			-- top
			section_mark (section_top, HEADER);			
			iterate (packge.copper.top.lines, write_line'access);
			iterate (packge.copper.top.arcs, write_arc'access);
			iterate (packge.copper.top.circles, write_circle'access);
			iterate (packge.copper.top.polygons.solid, write_polygon'access);
			iterate (packge.copper.top.polygons.hatched, write_polygon'access);
			iterate (packge.copper.top.cutouts, write_cutout'access);			
			iterate (packge.copper.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);			
			iterate (packge.copper.bottom.lines, write_line'access);
			iterate (packge.copper.bottom.arcs, write_arc'access);
			iterate (packge.copper.bottom.circles, write_circle'access);
			iterate (packge.copper.bottom.polygons.solid, write_polygon'access);
			iterate (packge.copper.bottom.polygons.hatched, write_polygon'access);
			iterate (packge.copper.bottom.cutouts, write_cutout'access);
			iterate (packge.copper.bottom.texts, write_text'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_copper, FOOTER);
		end write_copper;

		use pac_text_placeholders;		
		procedure write_placeholder (cursor : in pac_text_placeholders.cursor) is begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;

		procedure write_silk_screen is 
			use type_silk_lines;
			use type_silk_arcs;
			use type_silk_circles;
			use pac_silk_polygons;
			use pac_silk_cutouts;
		begin
			section_mark (section_silk_screen, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.silk_screen.top.lines, write_line'access);
			iterate (packge.silk_screen.top.arcs, write_arc'access);
			iterate (packge.silk_screen.top.circles, write_circle'access);
			iterate (packge.silk_screen.top.polygons, write_polygon'access);
			iterate (packge.silk_screen.top.cutouts, write_cutout'access);
			iterate (packge.silk_screen.top.texts, write_text'access);
			iterate (packge.silk_screen.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.silk_screen.bottom.lines, write_line'access);
			iterate (packge.silk_screen.bottom.arcs, write_arc'access);
			iterate (packge.silk_screen.bottom.circles, write_circle'access);
			iterate (packge.silk_screen.bottom.polygons, write_polygon'access);
			iterate (packge.silk_screen.bottom.cutouts, write_cutout'access);
			iterate (packge.silk_screen.bottom.texts, write_text'access);
			iterate (packge.silk_screen.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_silk_screen, FOOTER);			
		end write_silk_screen;

		procedure write_assembly_documentation is 
			use type_doc_lines;
			use type_doc_arcs;
			use type_doc_circles;
			use pac_doc_polygons;
			use pac_doc_cutouts;
		begin
			section_mark (section_assembly_doc, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.assembly_documentation.top.lines, write_line'access);
			iterate (packge.assembly_documentation.top.arcs, write_arc'access);
			iterate (packge.assembly_documentation.top.circles, write_circle'access);
			iterate (packge.assembly_documentation.top.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.top.cutouts, write_cutout'access);
			iterate (packge.assembly_documentation.top.texts, write_text'access);
			iterate (packge.assembly_documentation.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.assembly_documentation.bottom.lines, write_line'access);
			iterate (packge.assembly_documentation.bottom.arcs, write_arc'access);
			iterate (packge.assembly_documentation.bottom.circles, write_circle'access);
			iterate (packge.assembly_documentation.bottom.polygons, write_polygon'access);
			iterate (packge.assembly_documentation.bottom.cutouts, write_cutout'access);			
			iterate (packge.assembly_documentation.bottom.texts, write_text'access);
			iterate (packge.assembly_documentation.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);
		end write_assembly_documentation;
		
		procedure write_keepout is 
			use type_keepout_lines;
			use type_keepout_arcs;
			use type_keepout_circles;
			use type_keepout_polygons;
			use pac_keepout_cutouts;
		begin
			section_mark (section_keepout, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.keepout.top.lines, write_line'access);
			iterate (packge.keepout.top.arcs, write_arc'access);
			iterate (packge.keepout.top.circles, write_circle'access);
			iterate (packge.keepout.top.polygons, write_polygon'access);
			iterate (packge.keepout.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.keepout.bottom.lines, write_line'access);
			iterate (packge.keepout.bottom.arcs, write_arc'access);
			iterate (packge.keepout.bottom.circles, write_circle'access);
			iterate (packge.keepout.bottom.polygons, write_polygon'access);			
			iterate (packge.keepout.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);			
		end write_keepout;

		procedure write_stop_mask is 
			use type_stop_lines;
			use type_stop_arcs;
			use type_stop_circles;
			use type_stop_polygons;
			use pac_stop_cutouts;
		begin
			section_mark (section_stop_mask, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stop_mask.top.lines, write_line'access);
			iterate (packge.stop_mask.top.arcs, write_arc'access);
			iterate (packge.stop_mask.top.circles, write_circle'access);
			iterate (packge.stop_mask.top.polygons, write_polygon'access);
			iterate (packge.stop_mask.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stop_mask.bottom.lines, write_line'access);
			iterate (packge.stop_mask.bottom.arcs, write_arc'access);
			iterate (packge.stop_mask.bottom.circles, write_circle'access);
			iterate (packge.stop_mask.bottom.polygons, write_polygon'access);			
			iterate (packge.stop_mask.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_stop_mask, FOOTER);			
		end write_stop_mask;

		procedure write_stencil is 
			use type_stencil_lines;
			use type_stencil_arcs;
			use type_stencil_circles;
			use type_stencil_polygons;
			use pac_stencil_cutouts;
		begin
			section_mark (section_stencil, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stencil.top.lines, write_line'access);
			iterate (packge.stencil.top.arcs, write_arc'access);
			iterate (packge.stencil.top.circles, write_circle'access);
			iterate (packge.stencil.top.polygons, write_polygon'access);
			iterate (packge.stencil.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stencil.bottom.lines, write_line'access);
			iterate (packge.stencil.bottom.arcs, write_arc'access);
			iterate (packge.stencil.bottom.circles, write_circle'access);
			iterate (packge.stencil.bottom.polygons, write_polygon'access);
			iterate (packge.stencil.bottom.cutouts, write_cutout'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);			
		end write_stencil;

		procedure write_route_restrict is 
			use type_route_restrict_lines;
			use type_route_restrict_arcs;
			use type_route_restrict_circles;
			use type_route_restrict_polygons;
			use pac_route_restrict_cutouts;
		begin
			section_mark (section_route_restrict, HEADER);

			iterate (packge.route_restrict.lines, write_line'access);
			iterate (packge.route_restrict.arcs, write_arc'access);
			iterate (packge.route_restrict.circles, write_circle'access);
			iterate (packge.route_restrict.polygons, write_polygon'access);
			iterate (packge.route_restrict.cutouts, write_cutout'access);

			section_mark (section_route_restrict, FOOTER);			
		end write_route_restrict;

		procedure write_via_restrict is 
			use type_via_restrict_lines;
			use type_via_restrict_arcs;
			use type_via_restrict_circles;
			use type_via_restrict_polygons;
			use pac_via_restrict_cutouts;
		begin
			section_mark (section_via_restrict, HEADER);

			iterate (packge.via_restrict.lines, write_line'access);
			iterate (packge.via_restrict.arcs, write_arc'access);
			iterate (packge.via_restrict.circles, write_circle'access);
			iterate (packge.via_restrict.polygons, write_polygon'access);			
			iterate (packge.via_restrict.cutouts, write_cutout'access);
			
			section_mark (section_via_restrict, FOOTER);			
		end write_via_restrict;

		procedure write_contour is -- about PCB contours
			use type_pcb_contour_lines;
			use type_pcb_contour_arcs;
			use type_pcb_contour_circles;
		begin
			section_mark (section_pcb_contours, HEADER);

			iterate (packge.pcb_contour.lines, write_line'access);
			iterate (packge.pcb_contour.arcs, write_arc'access);
			iterate (packge.pcb_contour.circles, write_circle'access);

			section_mark (section_pcb_contours, FOOTER);
		end write_contour;
		
		-- CS currently no need for plated millings not terminal related
-- 		procedure write_contour_plated is begin
-- 			section_mark (section_pcb_contour_plated, HEADER);
-- 
-- 			iterate (packge.pcb_contour_plated.lines, write_line'access);
-- 			iterate (packge.pcb_contour_plated.arcs, write_arc'access);
-- 			iterate (packge.pcb_contour_plated.circles, write_circle'access);
-- 
-- 			section_mark (section_pcb_contour_plated, FOOTER);
-- 		end write_contour_plated;

		procedure write_package_contour is begin
			section_mark (section_pac_3d_contours, HEADER);
			-- CS
			section_mark (section_pac_3d_contours, FOOTER);
		end write_package_contour;

		procedure write_terminals is
			use et_packages;
			use type_terminals;
			terminal_cursor : type_terminals.cursor := packge.terminals.first;

			procedure write_plated_millings (millings : in type_plated_millings) is begin
				section_mark (section_pad_millings, HEADER);
				write_polygon_segments (pac_shapes.type_polygon_base (millings));
				section_mark (section_pad_millings, FOOTER);
			end write_plated_millings;
			
		begin -- write_terminals
			section_mark (section_terminals, HEADER);
			
			while terminal_cursor /= type_terminals.no_element loop
				section_mark (section_terminal, HEADER);
				write (keyword => keyword_name, parameters => space & to_string (key (terminal_cursor)));
				write (keyword => keyword_assembly_technology, parameters => to_string (element (terminal_cursor).technology));
				write (keyword => keyword_position, parameters => position (element (terminal_cursor).position));
				
				case element (terminal_cursor).technology is
					when THT =>
						-- pad contour top
						section_mark (section_pad_contours_tht, HEADER);
						
						section_mark (section_top, HEADER);
						write_polygon_segments (pac_shapes.type_polygon_base (element (terminal_cursor).pad_shape_tht.top));
						section_mark (section_top, FOOTER);

						-- pad contour bottom
						section_mark (section_bottom, HEADER);
						write_polygon_segments (pac_shapes.type_polygon_base (element (terminal_cursor).pad_shape_tht.bottom));
						section_mark (section_bottom, FOOTER);
						
						section_mark (section_pad_contours_tht, FOOTER);

						-- copper width in inner layers
						write (keyword => keyword_width_inner_layers, 
							   parameters => to_string (element (terminal_cursor).width_inner_layers));
						
						-- A THT terminal can have a drilled or a milled hole:
						write (keyword => keyword_tht_hole, parameters => to_string (element (terminal_cursor).tht_hole));

						case element (terminal_cursor).tht_hole is
							when DRILLED => 
								write (keyword_drill_size, parameters => to_string (element (terminal_cursor).drill_size));
								
							when MILLED => 
								write_plated_millings (element (terminal_cursor).millings);
						end case;
						
					when SMT =>
						-- pad contour
						section_mark (section_pad_contours_smt, HEADER);
						write_polygon_segments (pac_shapes.type_polygon_base (element (terminal_cursor).pad_shape));
						section_mark (section_pad_contours_smt, FOOTER);
						
						write (keyword => et_pcb_coordinates.keyword_face, parameters => et_pcb_coordinates.to_string (element (terminal_cursor).face));
						write (keyword => keyword_stop_mask, parameters => to_string (element (terminal_cursor).stop_mask));
						write (keyword => keyword_solder_paste, parameters => to_string (element (terminal_cursor).solder_paste));	
				end case;


				section_mark (section_terminal, FOOTER);
				next (terminal_cursor);
			end loop;
			
			section_mark (section_terminals, FOOTER);
		end write_terminals;
		
	begin -- save_package
		log (text => et_packages.to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> et_packages.to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " package");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		write (keyword => keyword_description, wrap => true, 
			   parameters => to_string (packge.description));

		write (keyword => keyword_appearance, parameters => to_string (packge.appearance));
		write (keyword => keyword_assembly_technology, parameters => to_string (packge.technology));

		write_silk_screen;
		write_assembly_documentation;
		write_keepout;
		write_copper;
		write_stop_mask;
		write_stencil;
		write_route_restrict;
		write_via_restrict;
		write_contour; -- pcb contour plated
		-- write_contour_plated; -- pcb contour -- CS currently no need
		write_terminals; -- incl. pad properties, drill sizes, millings, ...

		-- 3D stuff
		if packge.appearance = REAL then
			null;
			--write_package_contour;  -- CS uncomment when 3d support available
		end if;

		-- write footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " package model file end");
		new_line;
		
		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;

	end save_package;

	
	procedure read_package (
	-- Opens the package file and stores the package in container et_packages.packages.
		file_name 		: in et_packages.type_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;
		use et_packages;
		use et_packages.pac_shapes;
		use et_pcb;
		
		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the package model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 7; -- incl. section init

		package stack is new general_rw.stack_lifo (
			item	=> type_section,
			max 	=> max_section_depth);

		function to_string (section : in type_section) return string is
		-- Converts a section like SEC_KEEPOUT to a string "keepout".
			len : positive := type_section'image (section)'length;
		begin
			return to_lower (type_section'image (section) (5..len));
		end to_string;

	-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

		-- Once the appearance has been read, a new package will be created where this 
		-- pointer is pointing at:
		packge					: access type_package;
		pac_appearance			: type_package_appearance := package_appearance_default;

		-- The description and technology will be assigned once the complete
		-- model has been read. See main of this procedure.
		pac_description			: type_package_description.bounded_string; 
		pac_technology			: type_assembly_technology := assembly_technology_default;
		
		signal_layers			: et_pcb_stack.type_signal_layers.set;

		pac_text				: et_packages.type_text_with_content;
		pac_text_placeholder	: et_packages.type_text_placeholder;


		terminal_position		: et_pcb_coordinates.geometry.type_position := origin_zero_rotation;

		tht_width_inner_layers	: et_packages.type_track_width := et_packages.type_track_width'first;
		tht_hole				: et_packages.type_terminal_tht_hole := et_packages.terminal_tht_hole_default;
		tht_drill_size			: et_packages.type_drill_size := et_packages.type_drill_size'first;
		tht_millings			: et_packages.type_plated_millings;

		terminal_name			: et_packages.type_terminal_name.bounded_string;
		terminal_technology		: et_packages.type_assembly_technology := et_packages.assembly_technology_default;
		tht_pad_shape			: et_packages.type_pad_outline_tht;		
		smt_pad_shape			: et_packages.type_pad_outline;

		smt_pad_face			: et_pcb_coordinates.type_face := et_pcb_coordinates.face_default;
		smt_stop_mask			: et_packages.type_stop_mask_status := et_packages.stop_mask_status_default;
		smt_solder_paste		: et_packages.type_solder_paste_status := et_packages.solder_paste_status_default;

		procedure build_terminal is 
		-- Assembles the elements of a terminal and appends the final terminal to the
		-- list of terminals of the package.
			cursor : type_terminals.cursor;
			inserted : boolean;
		begin
			case terminal_technology is
				when THT => 
					case tht_hole is
						when DRILLED =>

							type_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> DRILLED,
									drill_size			=> tht_drill_size,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									width_inner_layers	=> tht_width_inner_layers));

						when MILLED =>
							type_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> MILLED,
									millings			=> tht_millings,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									width_inner_layers	=> tht_width_inner_layers));
					end case;

				when SMT =>
					type_terminals.insert (
						container	=> packge.terminals,
						key			=> terminal_name, -- 1,4,16,H9
						position	=> cursor,
						inserted	=> inserted,
						new_item	=> (
							technology		=> SMT,
							tht_hole		=> terminal_tht_hole_default, -- not relevant here, see spec
							face			=> smt_pad_face,
							position		=> terminal_position,
							pad_shape		=> smt_pad_shape,
							stop_mask		=> smt_stop_mask,
							solder_paste	=> smt_solder_paste));

			end case;

			if not inserted then
				log (ERROR, "terminal" & to_string (terminal_name) 
					 & " already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next terminal
			terminal_position := origin_zero_rotation;
 			smt_pad_shape := (others => <>);
			smt_stop_mask := et_packages.stop_mask_status_default;
			smt_solder_paste := solder_paste_status_default;
			tht_pad_shape := (others => <>);
			tht_hole := terminal_tht_hole_default;
			tht_width_inner_layers := et_packages.type_track_width'first;
			tht_drill_size := type_drill_size'first;
			
		end build_terminal;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
				-- and finally assembled to actual objects:

				-- fill zones
				procedure append_silk_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
												fill_style 	=> SOLID,
												easing 		=> board_easing
											   ));

						when HATCHED =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												fill_style 	=> HATCHED,
												easing 		=> board_easing,
												hatching	=> board_hatching));
					end case;
					
					board_reset_polygon;
				end;

				procedure append_silk_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												fill_style	=> SOLID,
												easing		=> board_easing
											   ));

						when HATCHED =>
							pac_silk_polygons.append (
								container	=> packge.silk_screen.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching	=> board_hatching));
					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												easing		=> board_easing,
												fill_style	=> SOLID));

						when HATCHED =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching 	=> board_hatching));
					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												easing 		=> board_easing,
												fill_style	=> SOLID));

						when HATCHED =>
							pac_doc_polygons.append (
								container	=> packge.assembly_documentation.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with 
												fill_style	=> HATCHED,
												easing		=> board_easing,
												hatching	=> board_hatching));

					end case;
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_polygon_top is begin
					type_keepout_polygons.append (
						container	=> packge.keepout.top.polygons, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										filled	=> board_filled));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_polygon_bottom is begin
					type_keepout_polygons.append (
						container	=> packge.keepout.bottom.polygons, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										filled	=> board_filled));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							type_stencil_polygons.append (
								container	=> packge.stencil.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stencil_polygons.append (
								container	=> packge.stencil.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							type_stencil_polygons.append (
								container	=> packge.stencil.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stencil_polygons.append (
								container	=> packge.stencil.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stop_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.top.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;
				
				procedure append_stop_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing));

						when HATCHED =>
							type_stop_polygons.append (
								container	=> packge.stop_mask.bottom.polygons, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching));
					end case;

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_polygon_top is begin
					case board_fill_style is
						when SOLID =>
							pac_copper_polygons_solid.append (
								container	=> packge.copper.top.polygons.solid, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));

						when HATCHED =>
							pac_copper_polygons_hatched.append (
								container	=> packge.copper.top.polygons.hatched, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching_copper,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));
					end case;
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_polygon_bottom is begin
					case board_fill_style is
						when SOLID =>
							pac_copper_polygons_solid.append (
								container	=> packge.copper.bottom.polygons.solid, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> SOLID,
										easing		=> board_easing,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));

						when HATCHED =>
							pac_copper_polygons_hatched.append (
								container	=> packge.copper.bottom.polygons.hatched, 
								new_item	=> (pac_shapes.type_polygon_base (polygon) with
										fill_style	=> HATCHED,
										easing		=> board_easing,
										hatching	=> board_hatching_copper,
										width_min 	=> polygon_width_min,
										isolation	=> polygon_isolation));
					end case;
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_route_restrict_polygon is begin
					type_route_restrict_polygons.append (
						container	=> packge.route_restrict.polygons, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				procedure append_via_restrict_polygon is begin
					type_via_restrict_polygons.append (
						container	=> packge.via_restrict.polygons, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				-- cutout zones
				procedure append_silk_cutout_top is begin
					pac_silk_cutouts.append (
						container	=> packge.silk_screen.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_silk_cutout_bottom is begin
					pac_silk_cutouts.append (
						container	=> packge.silk_screen.bottom.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_cutout_top is begin
					pac_doc_cutouts.append (
						container	=> packge.assembly_documentation.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_assy_doc_cutout_bottom is begin
					pac_doc_cutouts.append (
						container	=> packge.assembly_documentation.bottom.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing));
					
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_cutout_top is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_keepout_cutout_bottom is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.bottom.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_cutout_top is begin
					pac_stencil_cutouts.append (
						container	=> packge.stencil.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stencil_cutout_bottom is begin
					pac_stencil_cutouts.append (
						container	=> packge.stencil.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_stop_cutout_top is begin
					pac_stop_cutouts.append (
						container	=> packge.stop_mask.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;
				
				procedure append_stop_cutout_bottom is begin
					pac_stop_cutouts.append (
						container	=> packge.stop_mask.bottom.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));

					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_cutout_top is begin
					et_packages.pac_copper_cutouts.append (
						container	=> packge.copper.top.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_copper_cutout_bottom is begin
					et_packages.pac_copper_cutouts.append (
						container	=> packge.copper.bottom.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with
										easing => board_easing));
										
					-- clean up for next polygon
					board_reset_polygon;
				end;

				procedure append_route_restrict_cutout is begin
					pac_route_restrict_cutouts.append (
						container	=> packge.route_restrict.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing,
										layers => signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

				procedure append_via_restrict_cutout is begin
					pac_via_restrict_cutouts.append (
						container	=> packge.via_restrict.cutouts, 
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing => board_easing,
										layers => signal_layers));

					-- clean up for next polygon
					board_reset_polygon;

					et_pcb_stack.type_signal_layers.clear (signal_layers);
				end;

			begin -- execute_section
				case stack.current is

					when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
						SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_TOP =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION => null;

							when SEC_PAD_CONTOURS_THT => 
								tht_pad_shape.top := (pac_shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;

							when others => invalid_section;
						end case;
						
					when SEC_BOTTOM =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION => null;

							when SEC_PAD_CONTOURS_THT =>
								tht_pad_shape.bottom := (pac_shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_lines.append (
											container	=> packge.copper.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_lines.append (
											container	=> packge.silk_screen.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> packge.assembly_documentation.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_lines.append (
											container	=> packge.stencil.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STOP_MASK =>
										type_stop_lines.append (
											container	=> packge.stop_mask.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_lines.append (
											container	=> packge.keepout.top.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;
										
									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_lines.append (
											container	=> packge.copper.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_lines.append (
											container	=> packge.silk_screen.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_lines.append (
											container	=> packge.assembly_documentation.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_lines.append (
											container	=> packge.stencil.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_STOP_MASK =>
										type_stop_lines.append (
											container	=> packge.stop_mask.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_lines.append (
											container	=> packge.keepout.bottom.lines, 
											new_item	=> (pac_shapes.type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;

									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);

									when others => invalid_section;
								end case;
								
							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_lines.append (
									container	=> packge.pcb_contour.lines,
									new_item	=> (pac_shapes.type_line (board_line) with null record));

								-- clean up for next line
								board_reset_line;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_lines.append (
									container	=> packge.route_restrict.lines,
									new_item	=> (pac_shapes.type_line (board_line) with
													layers	=> signal_layers));

								-- clean up for next line
								board_reset_line;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_lines.append (
									container	=> packge.via_restrict.lines,
									new_item	=> (pac_shapes.type_line (board_line) with
													layers	=> signal_layers));

								-- clean up for next line
								board_reset_line;
								et_pcb_stack.type_signal_layers.clear (signal_layers);
								
							when SEC_PAD_CONTOURS_SMT => add_polygon_line (board_line);

							when SEC_MILLINGS => add_polygon_line (board_line);

							when SEC_CONTOURS => add_polygon_line (board_line);
								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_arcs.append (
											container	=> packge.copper.top.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_arcs.append (
											container	=> packge.silk_screen.top.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> packge.assembly_documentation.top.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_arcs.append (
											container	=> packge.stencil.top.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STOP_MASK =>
										type_stop_arcs.append (
											container	=> packge.stop_mask.top.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_arcs.append (
											container	=> packge.keepout.top.arcs,
											new_item	=> (pac_shapes.type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										type_copper_arcs.append (
											container	=> packge.copper.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILK_SCREEN => 
										type_silk_arcs.append (
											container	=> packge.silk_screen.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_arcs.append (
											container	=> packge.assembly_documentation.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										type_stencil_arcs.append (
											container	=> packge.stencil.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_STOP_MASK =>
										type_stop_arcs.append (
											container	=> packge.stop_mask.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_KEEPOUT =>
										type_keepout_arcs.append (
											container	=> packge.keepout.bottom.arcs, 
											new_item	=> (pac_shapes.type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_arcs.append (
									container	=> packge.pcb_contour.arcs,
									new_item	=> (pac_shapes.type_arc (board_arc) with null record));

								-- clean up for next arc
								board_reset_arc;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_arcs.append (
									container	=> packge.route_restrict.arcs,
									new_item	=> (pac_shapes.type_arc (board_arc) with layers => signal_layers));

								-- clean up for next arc
								board_reset_arc;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_arcs.append (
									container	=> packge.via_restrict.arcs,
									new_item	=> (pac_shapes.type_arc (board_arc) with layers => signal_layers));

								-- clean up for next arc
								board_reset_arc;
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_PAD_CONTOURS_SMT => add_polygon_arc (board_arc);

							when SEC_MILLINGS => add_polygon_arc (board_arc);

							when SEC_CONTOURS => add_polygon_arc (board_arc);
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										et_packages.pac_copper_circles.append (
											container	=> packge.copper.top.circles, 
											new_item	=> board_make_copper_circle);

									when SEC_SILK_SCREEN => 
										type_silk_circles.append (
											container	=> packge.silk_screen.top.circles, 
											new_item	=> board_make_fillable_circle);
															
										board_reset_circle_fillable; -- clean up for next circle

									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> packge.assembly_documentation.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_STENCIL =>
										type_stencil_circles.append (
											container	=> packge.stencil.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_STOP_MASK =>
										type_stop_circles.append (
											container	=> packge.stop_mask.top.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_KEEPOUT =>
										type_keepout_circles.append (
											container	=> packge.keepout.top.circles,
											new_item	=> board_make_fillable_circle_solid);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER => -- NON-ELECTRIC !!

										et_packages.pac_copper_circles.append (
											container	=> packge.copper.bottom.circles, 
											new_item	=> board_make_copper_circle);

									when SEC_SILK_SCREEN => 
										type_silk_circles.append (
											container	=> packge.silk_screen.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										type_doc_circles.append (
											container	=> packge.assembly_documentation.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_STENCIL =>
										type_stencil_circles.append (
											container	=> packge.stencil.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_STOP_MASK =>
										type_stop_circles.append (
											container	=> packge.stop_mask.bottom.circles, 
											new_item	=> board_make_fillable_circle);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_KEEPOUT =>
										type_keepout_circles.append (
											container	=> packge.keepout.bottom.circles,
											new_item	=> board_make_fillable_circle_solid);

										board_reset_circle_fillable; -- clean up for next circle

									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED =>
								
								et_packages.type_pcb_contour_circles.append (
									container	=> packge.pcb_contour.circles,
									new_item	=> (pac_shapes.type_circle (board_circle) with null record));

								-- clean up for next circle
								board_reset_circle;
								
							when SEC_ROUTE_RESTRICT =>
								
								type_route_restrict_circles.append (
									container	=> packge.route_restrict.circles,
									new_item	=> (board_make_fillable_circle_solid with signal_layers));

								board_reset_circle_fillable; -- clean up for next circle
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_VIA_RESTRICT =>
								
								type_via_restrict_circles.append (
									container	=> packge.via_restrict.circles,
									new_item	=> (board_make_fillable_circle_solid with signal_layers));

								board_reset_circle_fillable; -- clean up for next circle
								et_pcb_stack.type_signal_layers.clear (signal_layers);

							when SEC_PAD_CONTOURS_SMT => add_polygon_circle (board_circle);

							when SEC_MILLINGS => add_polygon_circle (board_circle);

							when SEC_CONTOURS => add_polygon_circle (board_circle);
								
							when others => invalid_section;
						end case;

					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_polygon_top;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_top;
										
									when SEC_STENCIL =>
										append_stencil_polygon_top;
										
									when SEC_STOP_MASK =>
										append_stop_polygon_top;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_top;

									when SEC_COPPER =>
										append_copper_polygon_top;
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_polygon_bottom;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_bottom;
										
									when SEC_STENCIL =>
										append_stencil_polygon_bottom;
										
									when SEC_STOP_MASK =>
										append_stop_polygon_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_bottom;

									when SEC_COPPER =>
										append_copper_polygon_bottom;
										
									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT =>
								append_route_restrict_polygon;

							when SEC_VIA_RESTRICT =>
								append_via_restrict_polygon;

							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_cutout_top;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_top;
										
									when SEC_STENCIL =>
										append_stencil_cutout_top;
										
									when SEC_STOP_MASK =>
										append_stop_cutout_top;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_top;

									when SEC_COPPER =>
										append_copper_cutout_top;

									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										append_silk_cutout_bottom;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_cutout_bottom;
										
									when SEC_STENCIL =>
										append_stencil_cutout_bottom;
										
									when SEC_STOP_MASK =>
										append_stop_cutout_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_bottom;

									when SEC_COPPER =>
										append_copper_cutout_bottom;

									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT =>
								append_route_restrict_cutout;

							when SEC_VIA_RESTRICT =>
								append_via_restrict_cutout;
								
							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_FILL_ZONE => check_outline (polygon);
							when SEC_CUTOUT_ZONE => check_outline (polygon);
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_COPPER =>
										
										type_texts_with_content.append (
											container	=> packge.copper.top.texts,
											new_item	=> pac_text);

									when SEC_SILK_SCREEN =>

										type_texts_with_content.append (
											container	=> packge.silk_screen.top.texts,
											new_item	=> pac_text);


									when SEC_ASSEMBLY_DOCUMENTATION =>

										type_texts_with_content.append (
											container	=> packge.assembly_documentation.top.texts,
											new_item	=> pac_text);
										
									when SEC_STOP_MASK =>

										type_texts_with_content.append (
											container	=> packge.stop_mask.top.texts,
											new_item	=> pac_text);
										
									-- CS SEC_KEEPOUT
										
									when others => invalid_section;
								end case;

								-- clean up for next text
								pac_text := (others => <>);
								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER =>
										
										type_texts_with_content.append (
											container	=> packge.copper.bottom.texts,
											new_item	=> pac_text);

									when SEC_SILK_SCREEN =>

										type_texts_with_content.append (
											container	=> packge.silk_screen.bottom.texts,
											new_item	=> pac_text);


									when SEC_ASSEMBLY_DOCUMENTATION =>

										type_texts_with_content.append (
											container	=> packge.assembly_documentation.bottom.texts,
											new_item	=> pac_text);
										
									when SEC_STOP_MASK =>

										type_texts_with_content.append (
											container	=> packge.stop_mask.bottom.texts,
											new_item	=> pac_text);
										
									-- CS SEC_KEEPOUT
										
									when others => invalid_section;
								end case;
								
								-- clean up for next text
								pac_text := (others => <>);

							when others => invalid_section;
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.silk_screen.top.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.assembly_documentation.top.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);

							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.silk_screen.bottom.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										et_packages.pac_text_placeholders.append (
											container	=> packge.assembly_documentation.bottom.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);
								
							when others => invalid_section;
						end case;

					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS => 
								-- Now all elements of the terminal have been read
								-- and can be assembled to the final terminal:
								build_terminal;
								
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL => 
								check_outline (polygon);
								smt_pad_shape := (pac_shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_THT =>
						case stack.parent is
							when SEC_TERMINAL => check_outline (polygon);
							when others => invalid_section;
						end case;

					when SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL => 
								tht_millings := (pac_shapes.type_polygon_base (polygon) with null record);
								board_reset_polygon;
								
							when others => invalid_section;
						end case;
						
					when SEC_INIT => raise constraint_error;
						
				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string; -- [POLYGON
				section			: in type_section) -- SEC_FILL_ZONE
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
						end if;
						return true;

					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;

				else -- neither a section header nor footer
					return false;
				end if;
			end set;

		begin -- process_line
			if set (section_top, SEC_TOP) then null;			
			elsif set (section_bottom, SEC_BOTTOM) then null;								
			elsif set (section_line, SEC_LINE) then null;
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_silk_screen, SEC_SILK_SCREEN) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;			
			elsif set (section_copper, SEC_COPPER) then null;
			elsif set (section_stop_mask, SEC_STOP_MASK) then null;			
			elsif set (section_stencil, SEC_STENCIL) then null;			
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;			
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			elsif set (section_pad_contours_smt, SEC_PAD_CONTOURS_SMT) then null;
			elsif set (section_pad_contours_tht, SEC_PAD_CONTOURS_THT) then null;
			elsif set (section_pad_millings, SEC_MILLINGS) then null;			
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_terminals, SEC_TERMINALS) then null;
			elsif set (section_terminal, SEC_TERMINAL) then null;
			elsif set (section_fill_zone, SEC_FILL_ZONE) then null;
			elsif set (section_contours, SEC_CONTOURS) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "package line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_appearance then -- appearance real/virtual
								expect_field_count (line, 2);
								pac_appearance := to_appearance (f (line,2));

								-- Depending on the appearance we create a virtual or real package
								-- where pointer packge is pointing at:
								case pac_appearance is
									when REAL =>
										packge := new type_package' (
													appearance	=> REAL,
													others		=> <>);

									when VIRTUAL =>
										packge := new type_package' (
													appearance	=> VIRTUAL,
													others		=> <>);
								end case;
										
							elsif kw = keyword_description then -- description "blabla"
								expect_field_count (line, 2);
								pac_description := to_package_description (f (line,2));

							elsif kw = keyword_assembly_technology then -- technology SMT/THT
								expect_field_count (line, 2);
								pac_technology := to_assembly_technology (f (line,2));
								
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
						SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_COPPER | SEC_KEEPOUT | SEC_STOP_MASK | SEC_STENCIL | 
								SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_PAD_CONTOURS_THT => null;

							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_line (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_line (line);
										
									when SEC_PAD_CONTOURS_THT => read_board_line (line);

									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_line (line);

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 2..16
											
											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);
											signal_layers := to_layers (line);
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PAD_CONTOURS_SMT => read_board_line (line);

							when SEC_MILLINGS => read_board_line (line);

							when SEC_CONTOURS => read_board_line (line);
								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>

										if not read_board_arc (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_arc (line);
										
									when SEC_PAD_CONTOURS_THT => read_board_arc (line);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_arc (line);

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);

										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PAD_CONTOURS_SMT => read_board_arc (line);

							when SEC_MILLINGS => read_board_arc (line);

							when SEC_CONTOURS => read_board_arc (line);
								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));

												elsif kw = keyword_filled then -- filled yes/no
													expect_field_count (line, 2);													
													board_filled := to_filled (f (line, 2));

												elsif kw = keyword_fill_style then -- fill_style solid/hatched
													expect_field_count (line, 2);													
													board_fill_style := to_fill_style (f (line, 2));

												elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
													expect_field_count (line, 2);													
													board_hatching.line_width := to_distance (f (line, 2));

												elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
													expect_field_count (line, 2);													
													board_hatching.spacing := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_circle (line);
										
									when SEC_COPPER => -- NON-ELECTRIC !!
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));

												elsif kw = keyword_filled then -- filled yes/no
													expect_field_count (line, 2);													
													board_filled := to_filled (f (line, 2));

												elsif kw = keyword_fill_style then -- fill_style solid/hatched
													expect_field_count (line, 2);													
													board_fill_style := to_fill_style (f (line, 2));

												elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
													expect_field_count (line, 2);													
													board_hatching_copper.line_width := to_distance (f (line, 2));

												elsif kw = keyword_hatching_border_width then -- hatching_border_width 1.0
													board_hatching_copper.border_width := to_distance (f (line, 2));

												elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
													expect_field_count (line, 2);													
													board_hatching_copper.spacing := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_PAD_CONTOURS_THT => read_board_circle (line);
										
									when others => invalid_section;
								end case;

							when SEC_PCB_CONTOURS_NON_PLATED => read_board_circle (line);

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_circle (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										elsif kw = keyword_fill_style then -- fill_style solid/hatched
											expect_field_count (line, 2);													
											board_fill_style := to_fill_style (f (line, 2));

										elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
											expect_field_count (line, 2);													
											board_hatching.line_width := to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
											expect_field_count (line, 2);													
											board_hatching.spacing := to_distance (f (line, 2));

										elsif kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line);
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_PAD_CONTOURS_SMT => read_board_circle (line);

							when SEC_MILLINGS => read_board_circle (line);

							when SEC_CONTOURS => read_board_circle (line);
								
							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_KEEPOUT =>
										-- no parameters allowed here
										declare
											kw : string := f (line, 1);
										begin
											invalid_keyword (kw);
										end;
										
									when SEC_COPPER =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;
										
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);
										signal_layers := to_layers (line);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_FILL_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOP_MASK =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_fill_style then -- fill_style solid/hatched
												expect_field_count (line, 2);													
												board_fill_style := to_fill_style (f (line, 2));

											elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
												expect_field_count (line, 2);													
												board_hatching.line_width := to_distance (f (line, 2));

											elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
												expect_field_count (line, 2);													
												board_hatching.spacing := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_KEEPOUT =>
										declare
											kw : string := f (line, 1);
										begin
											if kw = keyword_filled then -- filled yes/no
												expect_field_count (line, 2);
												board_filled := to_filled (f (line, 2));
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when SEC_COPPER =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_fill_style then -- fill_style solid/hatched
												expect_field_count (line, 2);													
												board_fill_style := to_fill_style (f (line, 2));

											elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_corner_easing (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
												expect_field_count (line, 2);													
												board_hatching_copper.line_width := to_distance (f (line, 2));

											elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
												expect_field_count (line, 2);													
												board_hatching_copper.spacing := to_distance (f (line, 2));

											elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
												expect_field_count (line, 2);													
												board_hatching_copper.border_width := to_distance (f (line, 2));
												
											elsif kw = keyword_isolation then -- isolation 0.5
												expect_field_count (line, 2);
												polygon_isolation := to_distance (f (line, 2));

											elsif kw = keyword_min_width then -- min_width 0.5
												expect_field_count (line, 2);
												polygon_width_min := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;
										
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);
										board_filled := to_filled (f (line, 2));

									elsif kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);
										signal_layers := to_layers (line);

									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_FILL_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_COPPER | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK => -- CS SEC_KEEPOUT
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												pac_text.position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 1.000
												expect_field_count (line, 2);
												pac_text.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												pac_text.line_width := to_distance (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												pac_text.alignment := et_text.to_alignment (line, 2);
												
											elsif kw = keyword_content then -- content "blabla"
												expect_field_count (line, 2); -- actual content in quotes !
												pac_text.content := et_text.to_content (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
								
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
												expect_field_count (line, 7);

												-- extract position of note starting at field 2
												pac_text_placeholder.position := to_position (line, 2);

											elsif kw = et_text.keyword_size then -- size 1.000
												expect_field_count (line, 2);
												pac_text_placeholder.size := to_distance (f (line, 2));

											elsif kw = et_text.keyword_line_width then -- line_width 0.1
												expect_field_count (line, 2);
												pac_text_placeholder.line_width := to_distance (f (line, 2));

											elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
												expect_field_count (line, 5);

												-- extract alignment starting at field 2
												pac_text_placeholder.alignment := et_text.to_alignment (line, 2);
												
											elsif kw = keyword_meaning then -- meaning reference, value, purpose
												expect_field_count (line, 2);
												pac_text_placeholder.meaning := to_text_meaning (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS =>
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_name then -- name 1,2,H7
										expect_field_count (line, 2);
										terminal_name := to_terminal_name (f (line,2));

									elsif kw = keyword_assembly_technology then -- technology tht
										expect_field_count (line, 2);
										terminal_technology := to_assembly_technology (f (line,2));

									elsif kw = keyword_position then -- position x 12.7 y 3.0 rotation 0.0
										expect_field_count (line, 7);
										terminal_position := to_position (line,2);

									elsif kw = keyword_width_inner_layers then -- width_inner_layers 0.2
										expect_field_count (line, 2);
										tht_width_inner_layers := to_distance (f (line,2));

									elsif kw = keyword_tht_hole then -- hole drilled/milled
										expect_field_count (line, 2);
										tht_hole := to_tht_hole (f (line,2));

									elsif kw = keyword_drill_size then -- drill_size 0.8
										expect_field_count (line, 2);
										tht_drill_size := to_distance (f (line,2));
										
									elsif kw = et_pcb_coordinates.keyword_face then -- face top/bottom
										expect_field_count (line, 2);
										smt_pad_face := et_pcb_coordinates.to_face (f (line,2));

									elsif kw = keyword_stop_mask then -- stop_mask open/closed
										expect_field_count (line, 2);
										smt_stop_mask := to_stop_mask_status (f (line,2));

									elsif kw = keyword_solder_paste then -- solder_paste applied/none
										expect_field_count (line, 2);
										smt_solder_paste := to_solder_paste_status (f (line,2));
										
									else
										invalid_keyword (kw);
									end if;
								end;

							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT | SEC_PAD_CONTOURS_THT | SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;
						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;
		
		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_package
		log_indentation_up;
		log (text => "reading package " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- test if container et_pcb.packages already contains the package
		-- named "file_name". If so, there would be no need to read the file_name again.
		if type_packages.contains (packages, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open package file
			open (
				file => file_handle,
				mode => in_file, 
				name => expand (to_string (file_name)));

			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assign description and technology as they have been read earlier.
			packge.description := pac_description;
			packge.technology := pac_technology;

			-- Insert the package (accessed by pointer packge) in et_pcb.packages:
			type_packages.insert (
				container	=> packages, 
				key			=> file_name, -- libraries/packages/S_SO14.pac
				new_item	=> packge.all);

		end if;

		-- CS Check integrity of package (style guides, conventions ...)
		-- use function "last" to fetch latest package

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_package;

	
	
end pcb_rw;
