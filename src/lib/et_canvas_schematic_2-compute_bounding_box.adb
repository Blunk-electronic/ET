------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--              CANVAS FOR SCHEMATIC / COMPUTE BOUNDING BOX                 --
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
with ada.directories;
with ada.exceptions;				use ada.exceptions;

with ada.containers;

-- with et_pcb_coordinates;
-- with et_terminals;
-- with et_devices;					use et_devices;
-- 

with et_frames;
with et_schematic;					use et_schematic;
with et_nets;
with et_net_labels;


separate (et_canvas_schematic_2)


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
	
	-- The first primitive object encountered will be the
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
		-- use et_frames;
		
		b : type_area; -- the bounding-box of the frame

		-- Get the size of the frame:
		size : constant et_frames.type_frame_size := 
			element (active_module).frames.frame.size;

	begin
		b.width := type_distance_positive (size.x);
		b.height := type_distance_positive (size.y);

		-- CS: orientation (portrait/landscape) ?
		
		-- CS: set b.position with frame position
		-- currently it is default (0;0);
		-- In schematic probably useless because the lower
		-- left corner of the frame is always at (0;0) ?
		
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



	-- This procedure reads the schematic elements and computes
	-- the bounding-box thereof.
	-- NOTE: The flag "first_object" is not used here as the drawing
	-- frame has alread handled the flag:
	procedure parse_schematic is

		-- This procedure iterates through:
		-- 1. all nets
		-- 2. all strands on the active sheet
		-- 3. all net segments
		-- in order to compute the bounding box of the nets:
		procedure process_nets is

			procedure query_nets (
				module_name	: in pac_module_name.bounded_string;
				module		: in type_module)
			is
				use et_nets;
				use pac_nets;
				use pac_strands;
				use pac_net_segments;

				use et_net_labels;
				use pac_net_labels;


				procedure query_label (c : in pac_net_labels.cursor) is
					label : type_net_label renames element (c);
				begin
					null;
					-- CS: The text or label size must be inquired similar to
					-- the process of drawing texts (see package et_canvas.text).
				end query_label;
				
				
				procedure query_segment (c : in pac_net_segments.cursor) is
					segment : type_net_segment renames element (c);

					-- Compute the preliminary bounding-box of the segment:
					b : type_area := get_bounding_box (segment, net_line_width);
				begin
					if debug then
						put_line (to_string (type_line (segment)));
					end if;
					
					merge_areas (bbox_new, b);

					if debug then
						put_line ("processing labels ...");
					end if;
					
					iterate (segment.labels, query_label'access);
				end query_segment;
				
				
				procedure query_strand (c : in pac_strands.cursor) is
					strand : type_strand renames element (c);
				begin
					-- We are interested in the strands on the
					-- active sheet only:
					if get_sheet (c) = active_sheet then
						iterate (strand.segments, query_segment'access);
						null;
					end if;
				end query_strand;
				
										   
				procedure query_net (c : in pac_nets.cursor) is
					net : type_net renames element (c);					
				begin
					if debug then
						put_line (to_string (c));
						put_line ("processing strands on sheet" 
							& to_string (active_sheet) & " ...");
					end if;
					
					iterate (net.strands, query_strand'access);
				end query_net;

				
			begin
				iterate (module.nets, query_net'access);
			end query_nets;
		
									 
		begin
			if debug then
				put_line ("processing nets ...");
			end if;
			
			query_element (active_module, query_nets'access);
		end process_nets;



		-- This procedure iterates through:
		-- 1. all devices
		-- 2. all symbols on the active sheet
		procedure process_devices is


			procedure query_devices (
				module_name	: in pac_module_name.bounded_string;
				module		: in type_module)
			is
				use pac_devices_sch;
				use pac_units;


				procedure query_unit (c : in pac_units.cursor) is
					unit : type_unit renames element (c);
				begin					
					null;
				end query_unit;

				
				procedure query_device (c : in pac_devices_sch.cursor) is
					device : type_device_sch renames element (c);					
				begin
					if debug then
						put_line (to_string (c));
					end if;

					iterate (device.units, query_unit'access);
				end query_device;

				
			begin
				iterate (module.devices, query_device'access);
			end query_devices;

			
		begin
			if debug then
				put_line ("processing devices ...");
			end if;
			
			query_element (active_module, query_devices'access);
		end process_devices;

			
		
		
	begin
		null;
		-- CS
		process_nets;
		process_devices;
		
	end parse_schematic;
	
		

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
		-- Get the margin between outer border of the frame
		-- and the edge of the paper:
		margin : constant et_frames.type_border_width := 
			element (active_module).frames.frame.border_width;
		
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
	put_line ("compute_bounding_box (schematic)");

	-- The drawing frame is regarded as part of the model:
	parse_drawing_frame;
	
	parse_schematic;


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
