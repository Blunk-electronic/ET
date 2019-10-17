------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                PCB_RW                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with et_general;
with et_pcb_coordinates;
with et_string_processing;
with et_packages;
with et_pcb;
with et_pcb_stack;
with general_rw;				use general_rw;

package body pcb_rw is

	function to_position ( -- CS combine with next function to_position using the tag test ?
	-- Returns a type_point_2d in the the layout.
		line : in et_string_processing.type_fields_of_line; -- "start x 44.5 y 53.5"
		from : in positive)
		return et_pcb_coordinates.geometry.type_point is
		
		use et_general;
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use et_string_processing;

		point : et_pcb_coordinates.geometry.type_point; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				set (
					point	=> point,
					axis	=> X,
					value 	=> to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
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

		use et_general;
		use et_pcb_coordinates;
		use et_pcb_coordinates.geometry;
		use et_string_processing;
		
		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		point : et_pcb_coordinates.geometry.type_position; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_pos_x then
				set (point => point, axis => X, value => to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_pos_y then
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

	
	
-- BASIC GEOMETRIC OBJECTS USED IN PACKAGES AND BOARDS

	procedure add_polygon_line (line : in out type_board_line) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_lines;

		-- make a polygon line:
		l : type_polygon_line := (et_packages.shapes.type_line (line) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.lines, l);

		board_reset_line;
	end;
	
	procedure board_reset_line is begin board_line := (others => <>); end;

	
	procedure add_polygon_arc (arc : in out type_board_arc) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_arcs;

		-- make a polygon arc:
		a : type_polygon_arc := (et_packages.shapes.type_arc (arc) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.arcs, a);

		board_reset_arc;
	end;
	
	procedure board_reset_arc is begin board_arc := (others => <>); end;


	procedure add_polygon_circle (circle : in out type_board_circle) is
		use et_packages.shapes;
		use et_packages.shapes.pac_polygon_circles;

		-- make a polygon circle:
		c : type_polygon_circle := (et_packages.shapes.type_circle (circle) with others => <>);
	begin
		-- collect the polygon line 
		append (polygon.segments.circles, c);

		board_reset_circle;
	end;

	procedure board_reset_circle is begin board_circle := (others => <>); end;

	
	procedure read_board_line (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_line. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_pcb_coordinates.geometry;
		use et_packages.shapes;
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
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
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
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
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

		else
			invalid_keyword (kw);
		end if;
	end;

	function read_board_arc (line : et_string_processing.type_fields_of_line) return boolean is
	-- Reads start and end point of the board_arc. If the statement is invalid then it returns a false.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
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
		else
			return false;
		end if;
	end;

	
	procedure read_board_circle (line : et_string_processing.type_fields_of_line) is
	-- Reads start and end point of the board_circle. If the statement is invalid then an error issued.
		kw : string := f (line, 1);
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
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
		use et_packages.shapes;
		use et_pcb_coordinates.geometry;
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
		use et_packages.shapes;
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
		circle				: in et_packages.shapes.type_circle;
		filled				: in et_packages.shapes.type_filled;
		fill_style			: in et_packages.type_fill_style;
		circumfence_width	: in et_packages.type_general_line_width;
		hatching			: in et_packages.type_hatching)
		return et_packages.type_fillable_circle is

		use et_packages;
		use et_packages.shapes;

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
			circle 				=> shapes.type_circle (board_circle),
			filled				=> board_filled,
			fill_style			=> board_fill_style,
			circumfence_width	=> board_line_width,
			hatching			=> board_hatching);
	end;

	function board_make_fillable_circle_solid return et_packages.type_fillable_circle_solid is 
		use et_packages;
	begin
		return (et_packages.shapes.type_circle (board_circle) with board_filled);
	end;

	function board_make_copper_circle return et_packages.type_copper_circle is
		use et_packages;
		use et_packages.shapes;
	begin
		case board_filled is
			when NO =>
				return (shapes.type_circle (board_circle) with 
					filled			=> NO,
					fill_style		=> SOLID, -- don't care here
					border_width	=> board_line_width);

			when YES =>
				case board_fill_style is
					when SOLID =>
						return (shapes.type_circle (board_circle) with 
							filled		=> YES,
							fill_style	=> SOLID);

					when HATCHED =>
						return (shapes.type_circle (board_circle) with
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
		use et_packages.shapes;
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
	



end pcb_rw;
