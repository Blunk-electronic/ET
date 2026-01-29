------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW PACKAGES                             --
--                                                                          --
--                               B o d y                                    --
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
--   history of changes:
--
-- To Do:
--


with ada.text_io;					use ada.text_io;

with et_primitive_objects;			use et_primitive_objects;
with et_mirroring;					use et_mirroring;

with et_device_name;
with et_device_library;
with et_device_library.packages;		use et_device_library.packages;
with et_device_model_names;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_device_value;
with et_device_purpose;

with et_package_model_name;			use et_package_model_name;
with et_package_model;				use et_package_model;
with et_package_name;
with et_package_library;			use et_package_library;
with et_package_variant;
with et_terminals;

with et_devices_electrical;				use et_devices_electrical;
with et_devices_electrical.packages;	use et_devices_electrical.packages;
with et_devices_non_electrical;			use et_devices_non_electrical;

with et_board_holes;
with et_board_outline;

with et_pcb_signal_layers;				use et_pcb_signal_layers;

with et_board_text;

with et_canvas_board_devices;
with et_canvas_tool;

with et_display.board;				use et_display.board;
with et_colors;						use et_colors;
with et_colors.board;				use et_colors.board;
with et_design_rules_board;			use et_design_rules_board;

with et_text_content;				use et_text_content;
with et_alignment;

with et_conductor_segment;
with et_conductor_text.packages;

with et_fill_zones;					use et_fill_zones;
with et_fill_zones.packages;		use et_fill_zones.packages;

with et_route_restrict;				
with et_route_restrict.packages;

with et_via_restrict;
with et_via_restrict.packages;

with et_stopmask;
with et_stopmask.packages;

with et_stencil;
with et_silkscreen;
with et_assy_doc;
with et_keepout;					

with et_board_ops_signal_layers;	use et_board_ops_signal_layers;



separate (et_canvas_board)


procedure draw_packages is

	package_position : type_package_position;
	
	brightness : type_brightness := NORMAL;

	device_name 	: et_device_name.type_device_name;
	device_value	: et_device_value.pac_device_value.bounded_string;
	device_purpose	: et_device_purpose.pac_device_purpose.bounded_string;

	-- Placeholders for name, value, purpose:
	device_placeholders	: et_device_placeholders.packages.type_text_placeholders;

	
	
	
	procedure draw_package (
		packge 		: in type_package_model)
	is
		-- This flag is set if the package is on the bottom side of the board.
		-- In that case, EVERYTHING (except the origin
		-- and text placeholders) must be mirrored along the Y-axis.
		flip : boolean := false;
		mirror : type_mirror := MIRROR_NO;
		

		use pac_draw_contours;
		
		
		
		procedure draw_origin is 

			procedure draw is begin
				--put_line ("draw origin" & to_string (get_place (package_position));				
				set_color_origin (brightness);
				draw_origin ((get_place (package_position), 0.0));
			end;

		begin
			if flip then
				if device_origins_enabled (BOTTOM) then
					draw;
				end if;
			else
				if device_origins_enabled (TOP) then
					draw;
				end if;
			end if;
		end draw_origin;


		

		-- This function returns for a given text placeholder
		-- the related content:
		function placeholder_to_content (
			placeholder : in type_text_placeholder)
			return pac_text_content.bounded_string
		is 
			result : pac_text_content.bounded_string;

			use et_device_name;
			use et_device_value;
			use et_device_purpose;
		begin
			case placeholder.meaning is
				when NAME 		=> result := to_content (to_string (device_name));
				when VALUE		=> result := to_content (to_string (device_value));
				when PURPOSE	=> result := to_content (to_string (device_purpose));
			end case;
			
			return result;
		end placeholder_to_content;

		


		-- This procedure draws a placeholder and its content:
		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			use pac_text_placeholders;
			ph : type_text_placeholder renames element (c);

			-- Build the content of the placeholder:
			content : pac_text_content.bounded_string := placeholder_to_content (ph);


			-- This procedure converts the placeholder to a complete
			-- text and draws it at the position as specified by the
			-- anchor mode of the candidate placeholder:
			procedure build_text is
				use pac_draw_text;
				use pac_text_vectorized;

				-- A temporary text that will be drawn:
				text : type_text_fab_with_content := (type_text_fab (ph) with others => <>);

				-- This flag is required in order to restore
				-- the previous brightness in case the placeholder is
				-- to be drawn highlighted:
				restore_normal_brightness : boolean := false;				

			begin
				-- 1. If the whole package is to be highlighted, then
				--    the placeholder will also be drawn highlighted.
				-- 2. If the package is not selected but only the placeholder,
				--    then only the placeholder will be highlighted.
				--    The normal brightness must be restored once the
				--    placeholder has been drawn:
				if is_selected (text) then
					set_brightness (BRIGHT);
					restore_normal_brightness := true;
				end if;


				
				text.content := content;

				case get_anchor_mode (ph) is
					when RELATIVE =>
						-- If the placeholder is anchored relatively to the package,
						-- then the package position (incl. rotation) must be taken into account.
						draw_vector_text (text, mirror, get_position (package_position));

					when ABSOLUTE =>
						-- If the placeholder is anchored with absolute coordinates,
						-- then the coordinates must be converted back to relative coordinates.
						-- The text is assigned with the relative coordinates and then
						-- drawn relative to the package position.
						-- This seems cumbersome but it is required because draw_vector_text
						-- mirrors the text position properly if it is relative to a
						-- reference (the package position):
						set_place (text, get_relative_position (ph, package_position));
						draw_vector_text (text, mirror, get_position (package_position));
				end case;


				-- Restore the previous brightness if the placeholder
				-- has been drawn highlighted:
				if restore_normal_brightness then
					set_brightness (NORMAL);
				end if;

			end;

			
		begin
			-- We draw the placeholder only if it has content:
			-- if not is_empty (content) then
			-- CS: It seems better to draw the placeholder even if
			-- its content is empty. The operator must see the origin
			-- of the placeholder.
				build_text;
			-- end if;
		end query_placeholder;



			
		

		procedure draw_assy is
			use et_assy_doc;
			use pac_doc_lines;

			procedure query_line (c : in pac_doc_lines.cursor) is
				line : type_doc_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;
			
			
		begin
			if flip then
				if assy_doc_enabled (TOP) then
					set_color_assy_doc (TOP, brightness);
					packge.assy_doc.bottom.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

					device_placeholders.assy_doc.bottom.iterate (query_placeholder'access);
				end if;

				if assy_doc_enabled (BOTTOM) then
					set_color_assy_doc (BOTTOM, brightness);
					packge.assy_doc.top.lines.iterate (query_line'access);

					device_placeholders.assy_doc.top.iterate (query_placeholder'access);
				end if;

			else -- not flipped
				if assy_doc_enabled (TOP) then
					set_color_assy_doc (TOP, brightness);
					packge.assy_doc.top.lines.iterate (query_line'access);

					device_placeholders.assy_doc.top.iterate (query_placeholder'access);
				end if;

				if assy_doc_enabled (BOTTOM) then
					set_color_assy_doc (BOTTOM, brightness);
					packge.assy_doc.bottom.lines.iterate (query_line'access);

					device_placeholders.assy_doc.bottom.iterate (query_placeholder'access);
				end if;

			end if;
		end draw_assy;



		


		procedure draw_silkscreen is
			use et_silkscreen;
			use pac_silk_lines;

			procedure query_line (c : in pac_silk_lines.cursor) is
				line : type_silk_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;
			

			use pac_silk_arcs;

			procedure query_arc (c : in pac_silk_arcs.cursor) is
				arc : type_silk_arc renames element (c);
			begin
				draw_arc (
					arc			=> arc,
					pos			=> get_position (package_position),		  
					width		=> arc.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_arc;

			
		begin
			-- put_line ("draw_silkscreen");
			
			if flip then
				if silkscreen_enabled (TOP) then
					set_color_silkscreen (TOP, brightness);
					packge.silkscreen.bottom.lines.iterate (query_line'access);
					packge.silkscreen.bottom.arcs.iterate (query_arc'access);
					-- CS circles, zones

					device_placeholders.silkscreen.bottom.iterate (query_placeholder'access);
				end if;

				if silkscreen_enabled (BOTTOM) then
					set_color_silkscreen (BOTTOM, brightness);
					packge.silkscreen.top.lines.iterate (query_line'access);
					packge.silkscreen.top.arcs.iterate (query_arc'access);

					device_placeholders.silkscreen.top.iterate (query_placeholder'access);
				end if;

			else -- not flipped
				if silkscreen_enabled (TOP) then
					set_color_silkscreen (TOP, brightness);
					packge.silkscreen.top.lines.iterate (query_line'access);
					packge.silkscreen.top.arcs.iterate (query_arc'access);
					
					device_placeholders.silkscreen.top.iterate (query_placeholder'access);
				end if;

				if silkscreen_enabled (BOTTOM) then
					set_color_silkscreen (BOTTOM, brightness);
					packge.silkscreen.bottom.lines.iterate (query_line'access);
					packge.silkscreen.bottom.arcs.iterate (query_arc'access);
					
					device_placeholders.silkscreen.bottom.iterate (query_placeholder'access);
				end if;
			end if;
		end draw_silkscreen;


		

		

		procedure draw_stopmask is
			use et_stopmask;
			use pac_stop_lines;

			
			procedure query_line (c : in pac_stop_lines.cursor) is
				line : type_stop_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;
			
		begin
			-- put_line ("draw_stopmask");
			
			if flip then
				if stop_mask_enabled (TOP) then
					set_color_stop_mask (TOP, brightness);
					packge.stop_mask.bottom.lines.iterate (query_line'access);
					-- CS arcs, circles, zones, texts

				end if;

				if stop_mask_enabled (BOTTOM) then
					set_color_stop_mask (BOTTOM, brightness);
					packge.stop_mask.top.lines.iterate (query_line'access);
			
				end if;

			else -- not flipped
				if stop_mask_enabled (TOP) then
					set_color_stop_mask (TOP, brightness);
					packge.stop_mask.top.lines.iterate (query_line'access);
				end if;

				if stop_mask_enabled (BOTTOM) then
					set_color_stop_mask (BOTTOM, brightness);
					packge.stop_mask.bottom.lines.iterate (query_line'access);

				end if;

			end if;
		end draw_stopmask;





		procedure draw_stencil is
			use et_stencil;
			use pac_stencil_lines;

			
			procedure query_line (c : in pac_stencil_lines.cursor) is
				line : type_stencil_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;
			
		begin
			-- put_line ("draw_stencil");
			
			if flip then
				if stencil_enabled (TOP) then
					set_color_stencil (TOP, brightness);
					packge.stencil.bottom.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

				end if;

				if stencil_enabled (BOTTOM) then
					set_color_stencil (BOTTOM, brightness);
					packge.stencil.top.lines.iterate (query_line'access);
			
				end if;

			else -- not flipped
				if stencil_enabled (TOP) then
					set_color_stencil (TOP, brightness);
					packge.stencil.top.lines.iterate (query_line'access);
				end if;

				if stencil_enabled (BOTTOM) then
					set_color_stencil (BOTTOM, brightness);
					packge.stencil.bottom.lines.iterate (query_line'access);

				end if;

			end if;
		end draw_stencil;

		
		



		procedure draw_keepout is
			use et_keepout;
			use pac_keepout_zones;

			
			procedure query_zone (c : in pac_keepout_zones.cursor) is
				zone : type_keepout_zone renames element (c);
			begin
				draw_contour (
					contour	=> zone,
					pos		=> get_position (package_position),		  
					filled	=> NO,
					width	=> zero,
					mirror	=> mirror);

			end query_zone;

			
		begin
			-- put_line ("draw_keepout");
			
			if flip then
				if keepout_enabled (TOP) then
					set_color_keepout (TOP, brightness);
					packge.keepout.bottom.zones.iterate (query_zone'access);
					-- CS cutouts

				end if;

				if keepout_enabled (BOTTOM) then
					set_color_keepout (BOTTOM, brightness);
					packge.keepout.top.zones.iterate (query_zone'access);
			
				end if;

			else -- not flipped
				if keepout_enabled (TOP) then
					set_color_keepout (TOP, brightness);
					packge.keepout.top.zones.iterate (query_zone'access);
				end if;

				if keepout_enabled (BOTTOM) then
					set_color_keepout (BOTTOM, brightness);
					packge.keepout.bottom.zones.iterate (query_zone'access);

				end if;

			end if;
		end draw_keepout;

		



		procedure draw_route_restrict is 
			use et_route_restrict;
			use et_route_restrict.packages;
			
			use pac_route_restrict_lines;
			use pac_route_restrict_arcs;
			use pac_route_restrict_circles;
			use pac_route_restrict_zones;
			use pac_route_restrict_cutouts;


			procedure query_line (c : in pac_route_restrict_lines.cursor) is
				line : type_route_restrict_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> zero,
					mirror		=> mirror,
					do_stroke	=> true);
				-- CS do not stroke could be possible, because
				-- everything has the same color and linewidth.
			end query_line;


			procedure query_circle (c : in pac_route_restrict_circles.cursor) is
				circle : type_route_restrict_circle renames element (c);
			begin
				draw_circle (
					circle		=> circle,
					pos			=> get_position (package_position),		  
					width		=> zero,
					mirror		=> mirror,
					filled		=> NO,
					do_stroke	=> true);
				-- CS do not stroke could be possible, because
				-- everything has the same color and linewidth.
			end query_circle;


			
		begin
			-- put_line ("draw_route_restrict");

			-- The color is in all restrict layers the same:
			set_color_route_restrict (brightness);
			
			if flip then
				if route_restrict_layer_enabled (face_to_layer (TOP)) then
					packge.route_restrict.bottom.lines.iterate (query_line'access);
					packge.route_restrict.bottom.circles.iterate (query_circle'access);
					-- CS arcs, zones, cutout
				end if;

				if route_restrict_layer_enabled (face_to_layer (BOTTOM)) then
					packge.route_restrict.top.lines.iterate (query_line'access);
					packge.route_restrict.top.circles.iterate (query_circle'access);
					-- CS arcs, zones
				end if;

			else -- not flipped
				if route_restrict_layer_enabled (face_to_layer (TOP)) then
					packge.route_restrict.top.lines.iterate (query_line'access);
					packge.route_restrict.top.circles.iterate (query_circle'access);
					-- CS arcs, zones
				end if;

				if route_restrict_layer_enabled (face_to_layer (BOTTOM)) then
					packge.route_restrict.bottom.lines.iterate (query_line'access);
					packge.route_restrict.bottom.circles.iterate (query_circle'access);
					-- CS arcs, zones
				end if;

			end if;

			-- CS final stroke ?
		end draw_route_restrict;
	




		procedure draw_via_restrict is 
			use et_via_restrict;
			use et_via_restrict.packages;
			
			use pac_via_restrict_zones;
			use pac_via_restrict_cutouts;

			
			procedure query_zone (c : in pac_via_restrict_zones.cursor) is
				zone : type_via_restrict_zone renames element (c);
			begin
				draw_contour (
					contour	=> zone,
					pos		=> get_position (package_position),		  
					filled	=> NO,
					width	=> zero,
					mirror	=> mirror);

			end query_zone;

				
		begin
			-- put_line ("draw_via_restrict");

			-- The color is in all restrict layers the same:
			set_color_via_restrict (brightness);
			
			if flip then
				if via_restrict_layer_enabled (face_to_layer (TOP)) then
					packge.via_restrict.bottom.zones.iterate (query_zone'access);
					-- CS zones, cutout
				end if;

				if via_restrict_layer_enabled (face_to_layer (BOTTOM)) then
					packge.via_restrict.top.zones.iterate (query_zone'access);
				end if;

			else -- not flipped
				if via_restrict_layer_enabled (face_to_layer (TOP)) then
					packge.via_restrict.top.zones.iterate (query_zone'access);
				end if;

				if via_restrict_layer_enabled (face_to_layer (BOTTOM)) then
					packge.via_restrict.bottom.zones.iterate (query_zone'access);
				end if;

			end if;
		end draw_via_restrict;






		procedure draw_holes is 
			use et_board_holes;
			use pac_holes;

			
			procedure query_hole (c : pac_holes.cursor) is
				hole : type_hole renames element (c);
			begin
				draw_contour (
					contour	=> hole,
					pos		=> get_position (package_position),
					filled	=> NO,
					mirror	=> mirror,
					width	=> zero);

			end query_hole;
				
			
		begin
			if board_contour_enabled then
				set_color_outline; -- CS brightness ?

				packge.holes.iterate (query_hole'access);
			end if;
		end draw_holes;



		
		

		procedure draw_conductors is
			use et_conductor_segment;
			use pac_conductor_lines;

			procedure query_line (c : in pac_conductor_lines.cursor) is
				line : type_conductor_line renames element (c);
			begin
				draw_line (
					line		=> line,
					pos			=> get_position (package_position),		  
					width		=> line.width,
					mirror		=> mirror,
					do_stroke	=> true);
			end query_line;


			

			use et_conductor_text.packages;
			use pac_conductor_texts;

			procedure query_text (c : in pac_conductor_texts.cursor) is
				use et_conductor_text;
				t : type_conductor_text renames element (c);

				use pac_draw_text;
			begin
				draw_vector_text (t, mirror, get_position (package_position));
			end query_text;

			
		begin
			-- put_line ("draw_conductors");
			
			if flip then
				if conductor_enabled (face_to_layer (TOP)) then
					set_color_conductor (face_to_layer (TOP), brightness);

					packge.conductors.bottom.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

					packge.conductors.bottom.texts.iterate (query_text'access);
				end if;

				if conductor_enabled (face_to_layer (BOTTOM)) then
					set_color_conductor (face_to_layer (BOTTOM), brightness);

					packge.conductors.top.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

					packge.conductors.top.texts.iterate (query_text'access);					
				end if;

			else -- not flipped
				if conductor_enabled (face_to_layer (TOP)) then
					set_color_conductor (face_to_layer (TOP), brightness);
					
					packge.conductors.top.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

					packge.conductors.top.texts.iterate (query_text'access);					
				end if;

				if conductor_enabled (face_to_layer (BOTTOM)) then
					set_color_conductor (face_to_layer (BOTTOM), brightness);

					packge.conductors.bottom.lines.iterate (query_line'access);
					-- CS arcs, circles, zones

					packge.conductors.bottom.texts.iterate (query_text'access);
				end if;

			end if;
		end draw_conductors;
		

		
		
		-- This procedure draws the terminals of the package.
		-- It draws:
		-- - the conducting area
		-- - the stopmask opening
		-- - the stencil opening (NOTE: THT pads do not have a stencil opening !)
		-- - the name of the terminal
		procedure draw_terminals is

			use et_terminals;
			use pac_terminals;


			-- Draws a single terminal candidate:
			procedure query_terminal (
				c : in pac_terminals.cursor) 
			is begin
				-- Due to the complexity of this procedure
				-- it is in a separate package:
				draw_terminal (
					name				=> key (c), -- like H5, 5, 3
					terminal			=> element (c),
					brightness			=> brightness,
					package_position	=> package_position,
					mirror				=> mirror,
					flip				=> flip);
								
			end query_terminal;

			
		begin
			packge.terminals.iterate (query_terminal'access);
		end draw_terminals;

		
		
	begin
		--put_line ("draw_package");

		-- Set the "flip" flag if the package is on the backside of the board:
		if is_flipped (package_position) then
			flip := true;
			mirror := MIRROR_ALONG_Y_AXIS;
		end if;
		
		draw_origin;
		draw_silkscreen;
		draw_assy;
		draw_stopmask; -- non-terminal related
		draw_stencil; -- non-terminal related
		draw_keepout; 
		draw_route_restrict;
		draw_via_restrict;
		draw_holes;
		
 		draw_conductors; -- NON-TERMINAL RELATED, NON-ELECTRICAL
		draw_terminals; -- pins, pads, plated millings
				
	end draw_package;


	

	
	use et_device_name;


	


	-- This procedure draws the package of an
	-- electrical device:
	procedure query_electrical_device (
		name	: in type_device_name;
		device	: in type_device_electrical)
	is
		use pac_package_models;
		package_model_name : pac_package_model_file.bounded_string;
	begin
		-- put_line ("device " & to_string (name));

		-- Here we address only real devices (which have 
		-- a physical representation in the board):
		if is_real (device) then

			-- If the device is selected then draw it highlighted:
			if is_selected (device) then
				brightness := BRIGHT;
			else
				brightness := NORMAL;
			end if;


			-- Fetch the complete position of the device
			-- (incl. x/y/rotaton/face) from the database:
			package_position := et_devices_electrical.packages.get_position (device);

			if is_moving (device) then
				-- Override package position by tool position:
				package_position.place := get_object_tool_position;
			end if;

			
			device_name := name;
			device_value := device.value;
			device_purpose := device.purpose;
			device_placeholders := device.placeholders;
			
			-- Get the name of the package model
			-- according to the package variant:
			package_model_name := get_package_model (
				device.model_cursor, device.variant);

			-- Send the actual package model to the draw procedure:
			draw_package (element (
				package_library, package_model_name));

		end if;
	end query_electrical_device;




	
	-- This procedure draws the package of a
	-- non-electrical device:
	procedure query_non_electrical_device (
		name	: in type_device_name;
		device	: in type_device_non_electrical)
	is 
		use pac_package_models;
	begin
		-- put_line ("device " & to_string (name));	

		-- If the device is selected then draw it highlighted:
		if is_selected (device) then
			brightness := BRIGHT;
		else
			brightness := NORMAL;
		end if;

		-- Fetch the complete position of the device
		-- (incl. x/y/rotaton/face) from the database:
		package_position := get_position (device);
		
		if is_moving (device) then
			-- Override package position by tool position:
			package_position.place := get_object_tool_position;
		end if;

		
		device_name := name;
		device_value := device.value;
		device_purpose := device.purpose;
		device_placeholders := device.placeholders;
		
		-- Send the actual package model to the draw procedure:
		draw_package (element (device.model_cursor));
	end query_non_electrical_device;



	
	
	-- This procedure queries the active module and iterates
	-- through the electrical and non-electrical devices:
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module)
	is
		debug : boolean := false;
		
		use pac_devices_electrical;
		cursor_electrical : pac_devices_electrical.cursor := 
			module.devices.first;

		use pac_devices_non_electrical;
		cursor_non_electrical : pac_devices_non_electrical.cursor := 
			module.devices_non_electric.first;

	begin
		if debug then
			put_line (" electrical devices");
		end if;

		
		-- Iterate electrical devices:
		while has_element (cursor_electrical) loop
			query_element (cursor_electrical, query_electrical_device'access);
			next (cursor_electrical);
		end loop;


		if debug then
			put_line (" non-electrical devices");
		end if;

		
		-- Iterate non-electrical devices:
		while has_element (cursor_non_electrical) loop
			query_element (cursor_non_electrical, query_non_electrical_device'access);
			next (cursor_non_electrical);
		end loop;

	end query_module;




	
	procedure draw_device_being_added is
		use et_canvas_board_devices;
		use pac_package_models;
	begin
		if device_add.valid then
			device_name := device_add.device_pre;
			set_place (package_position, get_primary_tool_position);
			set_rotation (package_position, device_add.rotation);
			
			draw_package (element (device_add.packge));
		end if;
	end draw_device_being_added;
	

	
begin
-- 	put_line ("draw packages ...");


	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_module'access);


	-- Draw the non-electrical device being added.
	-- If no device is being added, then nothing happens here:
	draw_device_being_added;
	
end draw_packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
