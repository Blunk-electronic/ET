------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                  CANVAS FOR BOARD / COMPUTE BOUNDING BOX                 --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.exceptions;				use ada.exceptions;
with ada.containers;

with et_frames;

-- with et_pcb_stack;
-- with et_design_rules;
with et_pcb_contour;

with et_text;


with et_undo_redo;

separate (et_canvas_board_2)

	
procedure compute_bounding_box (
	abort_on_first_error	: in boolean := false;
	ignore_errors			: in boolean := false;
	test_only				: in boolean := false)		
is
	-- debug : boolean := false;
	debug : boolean := true;

	-- In order to detect whether the bounding-box has
	-- changed we take a copy of the current bounding-box:
	bbox_old : type_area := bounding_box;

	-- This is the temporary bounding-box we are going to build
	-- in the course of this procedure:
	bbox_new : type_area;
	
	-- The first primitie object encountered will be the
	-- seed for bbox_new. All other objects cause 
	-- this bbox_new to expand. After the first object,
	-- this flag is cleared:
	first_object : boolean := true;


	-- This procedure uses the size of the drawing frame
	-- and adds it to the temporary bounding-box bbox_new.
	-- The size expresses the outer border of the frame.
	-- So, tt is sufficient to look at the size only because
	-- all other objects of the frame are definitely inside
	-- the outer border:
	procedure parse_drawing_frame is
		use et_frames;
		
		b : type_area; -- the bounding-box of the frame

		-- Get the size of the frame:
		size : constant type_frame_size := 
			element (active_module).board.frame.frame.size;

		-- Get the position of the frame:
		p : constant et_frames.type_position :=
			get_position (element (active_module).board.frame.frame);
		
		use pac_drawing_frame;
	begin
		-- Set width and height of the bounding-box:
		b.width  := to_distance (size.x);
		b.height := to_distance (size.y);
		
		-- CS: orientation (portrait/landscape) ?
		
		-- Set the position of the bounding-box:
		b.position := to_vector (p);
		
		-- If this is the first primitive object,
		-- then use its bounding-box as seed to start from:
		if first_object then
			bbox_new := b;
			first_object := false;
		else
		-- Otherwise, merge the box b with the box being built:
			merge_areas (bbox_new, b);
		end if;
			
	end parse_drawing_frame;


	

	-- This procedure parses all objects of the board database (or layout drawing).
	-- All objects are processed regardless whether they are displayed or not:
	procedure parse_board is


		-- This procedure parses the outer contour of the board
		-- and the holes (which can be regarded as inner contour):
		procedure process_board_outline is
			use et_pcb_contour;

			-- Outer contour:
			procedure process_outline is
				
				procedure query_outline_segments (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module)
				is 
					use pac_contours; -- instance of generic package
					use pac_segments;

					-- The bounding box of a single segment:
					b : type_area;
					
					procedure query_segment (c : in pac_segments.cursor) is 
						s : type_segment renames element (c);
					begin
						case s.shape is
							when LINE =>
								-- Compute bounding box of line segment:
								b := get_bounding_box (s.segment_line, pcb_contour_line_width);

							when ARC =>
								-- Compute bounding box of arc segment:
								b := get_bounding_box (s.segment_arc, pcb_contour_line_width);
						end case;
						merge_areas (bbox_new, b);
					end query_segment;
					
				begin
					-- If the outer contour is just a circle:
					if module.board.contours.outline.contour.circular then
						-- Compute bounding box of the single circle segment:
						b := get_bounding_box (
							module.board.contours.outline.contour.circle,
							pcb_contour_line_width);

						merge_areas (bbox_new, b);
					else
						-- If the outer contour is composed of lines and arcs:
						iterate (
							module.board.contours.outline.contour.segments,
							query_segment'access);
					end if;
				end query_outline_segments;

				
			begin
				if debug then
					put_line ("processing board outline ...");
				end if;

				query_element (active_module, query_outline_segments'access);
			end process_outline;



			

			-- Inner contour:
			procedure process_holes is

				procedure query_holes (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module) 
				is
					use pac_holes;

		
					procedure query_hole (c : in pac_holes.cursor) is 
						h : type_hole renames element (c);
					begin
						if h.contour.circular then
							null;

							-- draw_circle (
							-- 	circle	=> element (c).contour.circle,
							-- 	filled	=> NO, -- holes are never filled
							-- 	width	=> 0.0); -- don't care
							
						else
							null;
							-- iterate (element (c).contour.segments, query_segment'access);
						end if;
					end query_hole;
					
				begin
					iterate (module.board.contours.holes, query_hole'access);
				end query_holes;

				
			begin
				if debug then
					put_line ("processing board holes ...");
				end if;

				query_element (active_module, query_holes'access);				
			end process_holes;


			
		begin
			process_outline;
			process_holes;
		end process_board_outline;

		
	begin
		process_board_outline;

		-- CS

	end parse_board;

		
	-- This procedure updates the bounding-box and
	-- sets the bounding_box_changed flag
	-- in NON-TEST-MODE (which is default by argument test_only).
	-- In TEST-mode the bounding_box_changed flag is cleared:
	procedure update_global_bounding_box is begin
		if test_only then
			put_line ("TEST ONLY mode. Bounding-box not changed.");
			bounding_box_changed := false;
		else
			-- Update the global bounding-box:
			bounding_box := bbox_new;

			-- The new bounding-box differs from the old one.
			-- Set the global flag bounding_box_changed:
			bounding_box_changed := true;
		end if;
	end update_global_bounding_box;
	


	procedure add_margin is
		-- use et_frames;

		-- Get the margin between outer border of the frame
		-- and the edge of the paper:
		margin : constant et_frames.type_border_width := 
			element (active_module).board.frame.frame.border_width;
		
		-- The offset due to the margin:
		margin_offset : type_vector_model;
	begin
		bbox_new.width  := bbox_new.width  + 2.0 * type_distance_positive (margin);
		bbox_new.height := bbox_new.height + 2.0 * type_distance_positive (margin);
		
		-- Since we regard the margin as inside the bounding-box,
		-- we must move the bounding-box position towards bottom-left
		-- by the inverted margin_offset:
		margin_offset := (
			x => type_distance_positive (margin),
			y => type_distance_positive (margin));
		
		move_by (bbox_new.position, invert (margin_offset));
	end add_margin;

	
begin
	put_line ("compute_bounding_box (board)");

	-- The drawing frame is regarded as part of the model:
	parse_drawing_frame;
	
	parse_board;

	
	
	-- The temporary bounding-box bbox_new in its current
	-- state is the so called "inner bounding-box" (IB).

	-- Now, we expand the temporary bounding-box by the margin.
	-- The area around the drawing frame frame is regarded
	-- as part of the model and thus inside the bounding-box:
	add_margin;
	-- Now, bbox_new has become the "outer bounding-box" (OB).
	
	-- Compare the new bounding-box with the old 
	-- bounding-box to detect a change:
	if bbox_new /= bbox_old then

		-- Do the size check of the new bounding-box. If it is
		-- too large, then restore the old bounding-box:
		if bbox_new.width  >= bounding_box_width_max or
			bbox_new.height >= bounding_box_height_max then

			-- output limits and computed box dimensions:
			put_line ("WARNING: Bounding-box size limit exceeded !");
			put_line (" max. width : " 
				& to_string (bounding_box_width_max));
			
			put_line (" max. height: " 
				& to_string (bounding_box_height_max));
			
			put_line (" detected   : " 
				& to_string (bbox_new));

			-- Set the error flag:
			bounding_box_error := (
				size_exceeded => true,
				width  => bbox_new.width,
				height => bbox_new.height);

			
			if ignore_errors then
				put_line (" Errors ignored !");
				
				-- Override old global bounding-box with
				-- the faulty box bbox_new:
				update_global_bounding_box;
				
			else -- By default errors are NOT ignored.
				put_line (" Discarded. Global bounding-box NOT changed.");
				
				-- Clear the global flag bounding_box_changed
				-- because we discard the new bounding-box (due to 
				-- a size error) and
				-- leave the current global bounding-box untouched:
				bounding_box_changed := false;

			end if;

			
		else -- size ok, no errors
			-- Reset error flag:
			bounding_box_error := (others => <>);

			update_global_bounding_box;
		end if;
		
		
	else -- No change. 
		-- Clear the global flag bounding_box_changed:
		bounding_box_changed := false;

		-- Reset error flag:
		bounding_box_error := (others => <>);
	end if;

	
	if debug then
		put_line ("bounding-box: " & to_string (bounding_box));

		if bounding_box_changed then
			put_line (" has changed");
		end if;
	end if;
	
end compute_bounding_box;



-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
