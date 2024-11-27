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
-- To Do:
--

with ada.text_io;					use ada.text_io;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.exceptions;				use ada.exceptions;
with ada.containers;

with et_frames;
with et_schematic;
with et_pcb;
with et_packages;
with et_terminals;
with et_pcb_stack;
with et_conductor_segment;
with et_conductor_text.packages;

with et_pcb_contour;
with et_device_query_board;
with et_mirroring;					use et_mirroring;
with et_text;
with et_silkscreen;


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



---------------------------------------------------------------------------------------
-- BOARD:

	

	-- This procedure parses all objects of the board database (or layout drawing).
	-- All objects are processed regardless whether they are displayed or not:
	procedure parse_board is


		-- This procedure parses the outer contour of the board
		-- and the holes (which can be regarded as inner contour):
		procedure process_board_outline is
			use et_pcb_contour;

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

			
			-- Outer contour:
			procedure process_outline is
				
				procedure query_outline_segments (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module)
				is begin
					-- If the outer contour is just a circle, then we
					-- compute the bounding box of this single segment:
					if module.board.contours.outline.contour.circular then
						
						b := get_bounding_box (
							module.board.contours.outline.contour.circle,
							pcb_contour_line_width);

						merge_areas (bbox_new, b);
					else
					-- If the outer contour is composed of lines and arcs,
					-- then for each segment a bounding box must be computed:	
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
						-- If the candiate hole is just a circle,
						-- then we compute the bounding box of this single
						-- segment:
						if h.contour.circular then
							b := get_bounding_box (h.contour.circle, pcb_contour_line_width);
							merge_areas (bbox_new, b);
						else
						-- If the candiate hole is a list of lines and arcs,
						-- then we must compute the bounding box of each segment:
							iterate (h.contour.segments, query_segment'access);
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



		
---------------------------------------------------------------------------------------
-- PACKAGES OF DEVICES:
		
		
		-- This procedure parses the packages of devices:
		procedure process_devices is
			use et_schematic;
			use et_pcb;

			
			procedure process_package (
				electric			: in boolean;
				device_electric		: in pac_devices_sch.cursor;
				device_non_electric	: in pac_devices_non_electric.cursor;					   
				flip				: in type_flipped)
			is
				use et_device_query_board;
				use et_packages;
				use pac_package_models;

				-- This cursor points to the package model
				-- in the package library:
				package_cursor : pac_package_models.cursor;

				-- This is the position of the package
				-- on the board (x/y)
				package_position : type_vector_model;

				-- This is the rotation of the package
				-- on the board)
				package_rotation : type_rotation;
				
				-- For each primitive object we will
				-- compute a temporary bounding-box:
				b : type_area;

				
			


				procedure process_conductors is
					use et_conductor_segment;
					use pac_conductor_lines;
					use pac_conductor_arcs;
					use pac_conductor_circles;

					use et_conductor_text.packages;
					use pac_conductor_texts;

					
					
					procedure query_line (c : in pac_conductor_lines.cursor) is
						line : type_conductor_line renames element (c);
					begin
						b := get_bounding_box (
							line		=> line,
							width		=> line.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));
						
						merge_areas (bbox_new, b);
					end query_line;


					procedure query_arc (c : in pac_conductor_arcs.cursor) is
						arc : type_conductor_arc renames element (c);
					begin
						b := get_bounding_box (
							arc 		=> arc,
							width		=> arc.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));

						merge_areas (bbox_new, b);
					end query_arc;

					
					procedure query_circle (c : in pac_conductor_circles.cursor) is
						circle : type_conductor_circle renames element (c);
					begin
						b := get_bounding_box (
							circle		=> circle,
							width		=> circle.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));

						merge_areas (bbox_new, b);
					end query_circle;

					
					procedure query_text (c : in pac_conductor_texts.cursor) is
						text : et_conductor_text.type_conductor_text renames element (c);
					begin
						null; -- CS 
						-- parse segments of text
						-- include origin of text ?
						-- merge_areas (bbox_new, b);
					end query_text;
					
					
					packge : type_package_model renames element (package_cursor);
					
				begin
					packge.conductors.top.lines.iterate (query_line'access);
					packge.conductors.bottom.lines.iterate (query_line'access);

					packge.conductors.top.arcs.iterate (query_arc'access);
					packge.conductors.bottom.arcs.iterate (query_arc'access);

					packge.conductors.top.circles.iterate (query_circle'access);
					packge.conductors.bottom.circles.iterate (query_circle'access);

					packge.conductors.top.texts.iterate (query_text'access);
					packge.conductors.bottom.texts.iterate (query_text'access);					
				end process_conductors;


				

				procedure process_silkscreen is
					use et_silkscreen;			
					use pac_silk_lines;
					use pac_silk_arcs;
					use pac_silk_circles;
					use pac_silk_contours;
					use pac_silk_texts;

					procedure query_line (c : in pac_silk_lines.cursor) is
						line : type_silk_line renames element (c);
					begin
						b := get_bounding_box (
							line		=> line,
							width		=> line.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));
						
						merge_areas (bbox_new, b);
					end query_line;


					procedure query_arc (c : in pac_silk_arcs.cursor) is
						arc : type_silk_arc renames element (c);
					begin
						b := get_bounding_box (
							arc 		=> arc,
							width		=> arc.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));

						merge_areas (bbox_new, b);
					end query_arc;

					
					procedure query_circle (c : in pac_silk_circles.cursor) is
						circle : type_silk_circle renames element (c);
					begin
						b := get_bounding_box (
							circle		=> circle,
							width		=> circle.width,
							offset_1	=> package_position,
							offset_2	=> origin,
							rotation	=> package_rotation,
							mirror		=> to_mirror_along_y_axis (flip));

						merge_areas (bbox_new, b);
					end query_circle;


					procedure query_contour (c : in pac_silk_contours.cursor) is 
						contour : type_silk_contour renames element (c);
					begin
						null; 
						-- CS
					end query_contour;

					
					procedure query_text (c : in pac_silk_texts.cursor) is
						text : type_silk_text renames element (c);
					begin
						null; -- CS 
						-- parse segments of text
						-- include origin of text ?
						-- merge_areas (bbox_new, b);
					end query_text;


					packge : type_package_model renames element (package_cursor);
					
				begin
					packge.silkscreen.top.lines.iterate (query_line'access);
					packge.silkscreen.bottom.lines.iterate (query_line'access);

					packge.silkscreen.top.arcs.iterate (query_arc'access);
					packge.silkscreen.bottom.arcs.iterate (query_arc'access);

					packge.silkscreen.top.circles.iterate (query_circle'access);
					packge.silkscreen.bottom.circles.iterate (query_circle'access);

					packge.silkscreen.top.contours.iterate (query_contour'access);
					packge.silkscreen.bottom.contours.iterate (query_contour'access);
					
					packge.silkscreen.top.texts.iterate (query_text'access);
					packge.silkscreen.bottom.texts.iterate (query_text'access);
				end process_silkscreen;
				

				
				
				procedure process_terminals is
					use et_terminals;
					use pac_terminals;
					
					
					procedure query_terminal (
						c : in pac_terminals.cursor)
					is 
						t : type_terminal renames element (c);

						use pac_contours;
						
						procedure query_segment (c : in pac_segments.cursor) is
							use pac_segments;
							s : type_segment renames element (c);
						begin
							-- put_line ("segment");							
							case s.shape is
								when LINE =>
									b := get_bounding_box (
										line		=> s.segment_line,
										width		=> 0.0,
										offset_1	=> t.position.place,
										offset_2	=> package_position,
										rotation	=> package_rotation,
										mirror		=> to_mirror_along_y_axis (flip));
									
								when ARC =>
									b := get_bounding_box (
										arc			=> s.segment_arc,
										width		=> 0.0,
										offset_1	=> t.position.place,
										offset_2	=> package_position,
										rotation	=> package_rotation,
										mirror		=> to_mirror_along_y_axis (flip));
							end case;

							merge_areas (bbox_new, b);
						end query_segment;

						
					begin -- query_terminal
						case t.technology is
							when THT =>
								-- CS: Currently we process only the pad shape on top
								-- and bottom side. In the future other things like
								-- stop mask expansion and
								-- inner restring should be processed also.
								
								if t.pad_shape_tht.top.contour.circular then
									b := get_bounding_box (
										circle		=> t.pad_shape_tht.top.contour.circle,
										width		=> 0.0,
										offset_1	=> t.position.place,
										offset_2	=> package_position,
										rotation	=> package_rotation,
										mirror		=> to_mirror_along_y_axis (flip));
									
									merge_areas (bbox_new, b);
								else
									t.pad_shape_tht.top.contour.segments.iterate (query_segment'access);
								end if;

								if t.pad_shape_tht.bottom.contour.circular then
									b := get_bounding_box (
										circle		=> t.pad_shape_tht.bottom.contour.circle,
										width		=> 0.0,
										offset_1	=> t.position.place,
										offset_2	=> package_position,
										rotation	=> package_rotation,
										mirror		=> to_mirror_along_y_axis (flip));

									merge_areas (bbox_new, b);
								else
									t.pad_shape_tht.bottom.contour.segments.iterate (query_segment'access);
								end if;

								
								
							when SMT =>
								-- CS: Currently we process only the pad shape
								-- In the future other things like
								-- stop mask expansion, stencil opening should be processed also.
								
								if t.pad_shape_smt.contour.circular then
									b := get_bounding_box (
										circle		=> t.pad_shape_smt.contour.circle,
										width		=> 0.0,
										offset_1	=> t.position.place,
										offset_2	=> package_position,
										rotation	=> package_rotation,
										mirror		=> to_mirror_along_y_axis (flip));
										
									merge_areas (bbox_new, b);
								else
									t.pad_shape_smt.contour.segments.iterate (query_segment'access);
								end if;
						end case;								
					end query_terminal;

					
				begin
					-- Iterate though the terminals of the package:
					element (package_cursor).terminals.iterate (query_terminal'access);
				end process_terminals;
					
				
			begin
				-- Depening on the nature of the device, we fetch
				-- the package model and the position on the board:
				
				if electric then
					-- Locate the package model in the package library:
					package_cursor := get_package_model (device_electric);

					declare
						-- Fetch the position of the package on the board:
						pos : type_package_position := get_position (device_electric);
					begin
						-- Extract x/y coordinates of the package:
						package_position := pos.place;

						-- Extract rotation of the package:
						package_rotation := get_rotation (pos);
					end;
				else
					-- Locate the package model in the package library:
					package_cursor := get_package_model (device_non_electric);

					declare
						-- Fetch the position of the package on the board:
						pos : type_package_position := get_position (device_non_electric);
					begin
						-- Extract x/y coordinates of the package:
						package_position := pos.place;

						-- Extract rotation of the package:
						package_rotation := get_rotation (pos);
					end;
				end if;

				process_conductors;
				process_terminals;
				process_silkscreen;
				
			end process_package;



			
			
			procedure process_electrical_devices is

				procedure query_module (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module) 
				is
					use pac_devices_sch;

					procedure query_device (
						device_cursor : in pac_devices_sch.cursor) 
					is
						device : type_device_sch renames element (device_cursor);
					begin
						if is_real (device_cursor) then
							process_package (
								electric			=> true,
								device_electric		=> device_cursor,
								device_non_electric	=> pac_devices_non_electric.no_element,
								flip				=> device.flipped);
							
						end if;
					end query_device;
					
				begin
					module.devices.iterate (query_device'access);
				end query_module;

				
			begin
				if debug then
					put_line ("processing electrical devices ...");
				end if;

				query_element (active_module, query_module'access);
			end process_electrical_devices;
			



			
			procedure process_non_electrical_devices is

				procedure query_module (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module) 
				is
					use et_pcb;
					use pac_devices_non_electric;

					procedure query_device (
						device_cursor : in pac_devices_non_electric.cursor) 
					is 
						device : type_device_non_electric renames element (device_cursor);
					begin
						process_package (
							electric			=> false,
							device_electric		=> pac_devices_sch.no_element,
							device_non_electric	=> device_cursor,
							flip				=> device.flipped);
					end query_device;
					
				begin
					module.devices_non_electric.iterate (query_device'access);
				end query_module;

			begin
				if debug then
					put_line ("processing non-electrical devices ...");
				end if;

				query_element (active_module, query_module'access);
			end process_non_electrical_devices;



			
		begin
			if debug then
				put_line ("processing device packages ...");
			end if;

			process_electrical_devices;
			process_non_electrical_devices;
		end process_devices;
			

		
	begin
		process_board_outline;

		process_devices;
		
		-- CS
		-- process_silkscreen
		-- process_assembly_doc
		-- process_keepout
		-- process_stopmask
		-- process_conductors (tracks, vias, zones)
		-- process_stencil

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
