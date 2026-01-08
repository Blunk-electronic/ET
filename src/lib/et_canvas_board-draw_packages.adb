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

with et_drills;						use et_drills;

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
with et_assembly_technology;		use et_assembly_technology;
with et_terminals;
with et_terminal_name;				use et_terminal_name;
with et_terminal_hole;				use et_terminal_hole;
with et_terminal_tht;				use et_terminal_tht;

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

with et_terminal_stencil;			use et_terminal_stencil;
with et_terminal_stopmask;			use et_terminal_stopmask;
with et_stopmask;
with et_stopmask.packages;

with et_stencil;
with et_silkscreen;
with et_assy_doc;
with et_keepout;					

with et_contour_to_polygon;			use et_contour_to_polygon;



separate (et_canvas_board)


procedure draw_packages is

	-- The deepest conductor layer towards bottom is defined by the layer stack:
	bottom_layer : constant type_signal_layer := 
		et_board_ops.get_deepest_conductor_layer (active_module);

	


	package_position : type_package_position;
	
	brightness : type_brightness := NORMAL;

	device_name 	: et_device_name.type_device_name;
	device_value	: et_device_value.pac_device_value.bounded_string;
	device_purpose	: et_device_purpose.pac_device_purpose.bounded_string;

	-- Placeholders for name, value, purpose:
	device_placeholders	: et_device_placeholders.packages.type_text_placeholders;

	

	-- Translates face (TOP/BOTTOM) to conductor layer 1/bottom_layer.
	function face_to_layer (f : in type_face) return type_signal_layer is begin
		case f is
			when TOP => return type_signal_layer'first;
			when BOTTOM => return bottom_layer;
		end case;
	end face_to_layer;


	
	
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
			procedure query_terminal (c : in pac_terminals.cursor) is
				
				-- The name of the terminal (like H5, 5, 3)
				name : constant string := to_string (key (c));
				
				t : type_terminal renames element (c);

				use et_board_geometry.pac_contours;

		

				

				-- This procedure draws the outer contour of the THT pad:
				procedure tht_outer_layer (
					pad_contours	: in type_contour; -- the outline of the solder pad
					pad_position	: in type_position; -- the center of the pad incl. its rotation
					drilled_milled	: in type_terminal_tht_hole;
					drill_size		: in type_drill_size := type_drill_size'first;
					hole_contours	: in type_contour := plated_millings_default)
				is
					procedure draw_conductor is
						c : type_circle;
						use pac_draw_contours;
					begin
						set_color_tht_pad (brightness);

						--put_line ("draw_conductor");
						
						case drilled_milled is
							when DRILLED =>									
								set_center (c, pad_position.place);
								set_radius (c, drill_size * 0.5);
								
								---put_line ("pad_pos" & to_string (pad_position));

								draw_contour_with_circular_cutout (
									outer_border	=> pad_contours,
									inner_border	=> c,
									pos				=> get_position (package_position),
									offset			=> pad_position,
									mirror			=> mirror);

								
							when MILLED =>

								draw_contour_with_arbitrary_cutout (
									outer_border	=> pad_contours,
									inner_border	=> hole_contours,
									pos				=> get_position (package_position),
									offset			=> pad_position,
									mirror			=> mirror);

									
						end case;
					end draw_conductor;

					
				begin
					draw_conductor;	
				end tht_outer_layer;

		

				procedure draw_tht_outer_layers_drilled is begin
					-- Draw the conductor shape of outer layers:
					if flip then
						if conductor_enabled (face_to_layer (TOP)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

					else -- no flip
						if conductor_enabled (face_to_layer (TOP)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								drill_size		=> t.drill_size);
						end if;

					end if;
				end draw_tht_outer_layers_drilled;
				


				-- Draws the conductor shape of outer layers:
				procedure draw_tht_outer_layers_milled is begin
					if flip then
						if conductor_enabled (face_to_layer (TOP)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);

						end if;
						
					else
						if conductor_enabled (face_to_layer (TOP)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.top,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;

						if conductor_enabled (face_to_layer (BOTTOM)) then
							tht_outer_layer (
								pad_contours	=> t.pad_shape_tht.bottom,
								pad_position	=> t.position,
								drilled_milled	=> t.tht_hole,
								hole_contours	=> t.millings);
						end if;
					end if;
				end draw_tht_outer_layers_milled;
				
				
				
				-- Draws the pad contour of a drilled THT pad:
				procedure draw_tht_inner_layers_drilled is 

					-- This procedure draws the pad contour of a drilled THT pad
					-- in an inner conductor layer if any inner conductor layer is enabled. 
					-- If no inner conductor layer is enabled, nothing happens.
					procedure do_it (
						drill_size		: in type_drill_size;
						restring		: in type_restring_width;
						pad_position	: in type_position) -- the center of the pad, rotation has no meaning
					is
						circle : type_circle;
						mirror_style : type_mirror := MIRROR_NO;
						
					begin
						if inner_conductors_enabled (bottom_layer) then
							
							-- Build a circle that represents
							-- the restring of inner layers:
							set_center (circle, pad_position.place);
							set_radius (circle, (drill_size + restring) * 0.5);

						
							-- Draw the restring:
							draw_circle (
								circle		=> circle, 
								pos			=> get_position (package_position), 
								filled		=> NO,
								width		=> restring,
								mirror		=> mirror,
								do_stroke	=> true);


							-- CS
							
							-- Draw the hole:
							-- set_color_background;
							
							-- The cutout area must clear out the outer area:
							-- set_operator (context, CAIRO_OPERATOR_CLEAR);

							-- circle.radius := drill_size * 0.5;
		
							-- draw_circle (
							-- 	circle		=> circle, 
							-- 	pos			=> get_position (package_position), 
							-- 	filled		=> YES,
							-- 	width		=> zero,
							-- 	mirror		=> mirror_style,
							-- 	do_stroke	=> true);


							-- restore default compositing operator:
							-- set_operator (context, CAIRO_OPERATOR_OVER);		
						end if;
					end do_it;

				begin
					do_it (
						drill_size		=> t.drill_size,
						restring		=> t.width_inner_layers,
						pad_position	=> t.position);
													
				end draw_tht_inner_layers_drilled;



				
				-- Draws the conductor contours of inner layers:
				procedure draw_tht_inner_layers_milled is 

					-- This procedure draws the pad contour of a milled THT pad
					-- in an inner conductor layer
					-- if any inner conductor layer is enabled. If no inner conductor
					-- layer is enabled, nothing happens.
					-- The pad contour is derived from the given hole contours:
					procedure do_it (
						hole_contours	: in type_contour; -- the contours of the milled hole
						restring_width	: in type_track_width;
						pad_position	: in type_position) -- the center of the pad incl. its rotation
					is
						use pac_geometry_2;	
						use et_board_geometry.pac_contours;
						use et_board_geometry.pac_polygons;
						use pac_offsetting;

						use pac_draw_contours;
						
						polygon_tmp : type_polygon;
						pad_contours : type_contour;
					begin
						if inner_conductors_enabled (bottom_layer) then

							-- Make a temporary polygon from the hole contours:
							polygon_tmp := to_polygon (hole_contours, fill_tolerance, EXPAND);
							-- CS: expand correct ?

							-- Offset the polygon so that it extends the given hole outline 
							-- by the restring_width:
							offset_polygon (
								polygon			=> polygon_tmp, 
								offset			=> type_float_model (restring_width),
								log_threshold	=> log_threshold + 80); -- CS

							-- convert the temporary polygon back to a contour
							pad_contours := to_contour (polygon_tmp);

							draw_contour_with_arbitrary_cutout (
								outer_border	=> pad_contours,
								inner_border	=> hole_contours,								   
								pos				=> get_position (package_position),
								offset			=> pad_position,
								mirror			=> mirror);
			
						end if;
					end do_it;


				begin
					do_it (
						hole_contours	=> t.millings,
						restring_width	=> t.width_inner_layers,
						pad_position	=> t.position);
					
				end draw_tht_inner_layers_milled;


				
				
				function get_stop_mask_expansion return type_stop_mask_expansion is  -- from DRU
					use et_canvas_schematic;
					use et_board_ops;
				begin
					return get_pcb_design_rules (active_module).stop_mask.expansion_min;
				end get_stop_mask_expansion;

				
				

				

				
				-- Draws the stopmask opening of a THT terminal:
				procedure draw_tht_stop_mask is 

					procedure draw_stopmask (
						pad_contours	: in type_contour; -- the outline of the solder pad
						stopmask		: in type_stopmask_shape; -- the stopmask in the outer layer
						pad_position	: in type_position) -- the center of the pad incl. its rotation
					is
						
						stopmask_contours : type_stopmask_contour;
						-- CS initialize (see build_contour)


						-- Builds the stopmask contour from the pad contour:
						procedure build_contour is
							use pac_geometry_2;	
							use et_board_geometry.pac_contours;
							use et_board_geometry.pac_polygons;
							use pac_offsetting;

							polygon_tmp : type_polygon;
						begin
							case stopmask.expand_mode is
								when AS_PAD =>
									-- Copy solder pad contours to stopmask without
									-- any modifications:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy solder pad contour to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									-- Expand the polygon according to DRU settings:
									offset_polygon (
										polygon			=> polygon_tmp,
										offset			=> type_float_model (get_stop_mask_expansion),
										log_threshold	=> log_threshold + 80); -- CS

									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>										
									-- Use the stopmask contours as given by the user:
									stopmask_contours := stopmask.contour;

							end case;
						end build_contour;

						
					begin
						build_contour;						
						
						-- Draw the outer contour of the stopmask opening:
						pac_draw_contours.draw_contour (
							contour		=> stopmask_contours,
							pos			=> get_position (package_position),
							offset		=> pad_position,
							filled		=> YES,
							width		=> zero,
							mirror		=> mirror);

					end draw_stopmask;

					
				begin
					-- put_line ("draw_tht_stop_mask");
					
					if flip then
						if stop_mask_enabled (TOP) then
							set_color_stop_mask (TOP, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.bottom,
								stopmask		=> t.stop_mask_shape_tht.bottom,
								pad_position	=> t.position);
						end if;
					
						if stop_mask_enabled (BOTTOM) then
							set_color_stop_mask (BOTTOM, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.top,
								stopmask		=> t.stop_mask_shape_tht.top,
								pad_position	=> t.position);
						end if;
								
					else -- not flipped
						if stop_mask_enabled (TOP) then
							set_color_stop_mask (TOP, brightness);
	
							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.top,
								stopmask		=> t.stop_mask_shape_tht.top,
								pad_position	=> t.position);
						end if;
					
						if stop_mask_enabled (BOTTOM) then
							set_color_stop_mask (BOTTOM, brightness);

							draw_stopmask (
								pad_contours	=> t.pad_shape_tht.bottom,
								stopmask		=> t.stop_mask_shape_tht.bottom,
								pad_position	=> t.position);
							
						end if;
					end if;
				end draw_tht_stop_mask;
				


				
				-- Draws the name of a THT pad if any conductor layer is enabled 
				procedure draw_name_tht (
					pad_position : in type_position)  -- the center of the pad
				is
					use et_alignment;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					if conductors_enabled then

						set_color_terminal_name (brightness);
						
						-- Rotate the pad POSITION about the origin
						-- of the package by the rotation of the package:
						rotate_by (pos_tmp, get_rotation (package_position));

						-- If the package is flipped, then the terminal POSITION
						-- must be mirrored along the Y axis.
						mirror_point (pos_tmp, mirror); 
						
						-- Now move the pad POSITION by the position
						-- of the package:
						add (pos_tmp, package_position.place);
						
						-- Draw the pad name at pos_tmp:							
						draw_text (
							content		=> to_content (name),
							size		=> terminal_name_size,
							font		=> terminal_name_font,
							anchor		=> pos_tmp,
							origin		=> false, -- no origin required
							rotation	=> zero_rotation,
							alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					end if;
				end draw_name_tht;
				



				-- Draws the name of an smt pad.
				-- The given position is the center of the pad
				-- relative to the origin of the package:
				procedure draw_name_smt (
					pad_position : in type_position)  -- the center of the pad
				is
					use et_alignment;

					-- Take a copy of the x/y position of the pad:
					pos_tmp : type_vector_model := pad_position.place;

					use pac_draw_text;
				begin
					set_color_terminal_name (brightness);

					-- Rotate the pad POSITION about the origin
					-- of the package by the rotation of the package:
					rotate_by (pos_tmp, get_rotation (package_position));

					-- If the package is to be flipped then
					-- mirror the pad POSITION along the Y-axis:
					mirror_point (pos_tmp, mirror);

					-- Now move the pad POSITION by the position
					-- of the package:
					add (pos_tmp, package_position.place);

					-- Draw the pad name at pos_tmp:
					draw_text (
						content		=> to_content (name),
						size		=> terminal_name_size,
						font		=> terminal_name_font,
						anchor		=> pos_tmp,
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (ALIGN_CENTER, ALIGN_CENTER));

						-- CS The rotation should be so that the
						-- name can be read from front and from the right.
					
				end draw_name_smt;
				

				

				-- This procedure draws the SMT pad, the stopmask, the stencil and 
				-- the terminal name. The terminal name will be drawn only if
				-- the signal layer is enabled.
				procedure draw_pad_smt (
					pad_contours	: in type_contour; -- the outline of the solder pad (copper)
					stopmask		: in type_stopmask_smt; -- the stopmask of the pad
					stencil			: in type_stencil_shape; -- the solder cream mask of the pad

					-- The position of the center of the pad (relative to the package position)
					pad_position	: in type_position; -- incl. pad rotation about itself
					f				: in type_face) -- the face where the pad is
				is
					use pac_draw_contours;
					
	
					
					-- Draws the conductor area of the pad:
					procedure draw_conductor is 

						-- Does the actual drawing of the conductor area:
						procedure do_it is begin
							draw_contour (
								contour	=> pad_contours,
								pos		=> get_position (package_position),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;
				

					begin
						if flip then
							if conductor_enabled (face_to_layer (TOP)) then
								set_color_conductor (face_to_layer (TOP), brightness);

								if f = BOTTOM then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;


							if conductor_enabled (face_to_layer (BOTTOM)) then
								set_color_conductor (face_to_layer (BOTTOM), brightness);

								if f = TOP then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;

							
						else -- not flipped

							if conductor_enabled (face_to_layer (TOP)) then
								set_color_conductor (face_to_layer (TOP), brightness);

								if f = TOP then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;


							if conductor_enabled (face_to_layer (BOTTOM)) then
								set_color_conductor (face_to_layer (BOTTOM), brightness);

								if f = BOTTOM then
									do_it;
								end if;
													
								-- draw the terminal name
								draw_name_smt (pad_position);
							end if;
							
						end if;
					end draw_conductor;
					

					
					
					-- Draws the stopmask of the pad:
					procedure draw_stopmask is 
						
						stopmask_contours : type_stopmask_contour;
						-- CS initialize (see procedure build_contour)
						
						
						-- Builds the stopmask contour from the pad contour:
						procedure build_contour is
							use pac_geometry_2;	
							use et_board_geometry.pac_contours;
							use et_board_geometry.pac_polygons;
							use pac_offsetting;

							polygon_tmp : type_polygon;
						begin
							case stopmask.expand_mode is
								when AS_PAD =>
									-- Copy pad contours to stopmask without
									-- any modification:
									stopmask_contours := (type_contour (pad_contours) with null record);

									
								when EXPAND_PAD =>
									-- Copy pad contours to stopmask:
									stopmask_contours := (type_contour (pad_contours) with null record);

									-- Now the stopmask must be expanded according to the DRU settings.

									-- Make a temporary polygon from the stopmask contours:
									polygon_tmp := to_polygon (stopmask_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?

									-- Offset the temporary polygon:
									offset_polygon (
										polygon			=> polygon_tmp,
										offset			=> type_float_model (get_stop_mask_expansion), -- from DRU
										log_threshold	=> log_threshold + 80); -- CS
									
									-- Convert the temporary polygon back to a contour:
									stopmask_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stopmask contour as given by the user settings:
									stopmask_contours := stopmask.contour;
							end case;
						end build_contour;


						
						-- Does the actual drawing of the stopmask contour:
						procedure do_it is begin
							draw_contour (
								contour	=> stopmask_contours,
								pos		=> get_position (package_position),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;

						
					begin	
						build_contour;


						if flip then
							if stop_mask_enabled (TOP) then
								set_color_stop_mask (TOP, brightness);

								if f = BOTTOM then
									do_it;
								end if;
							end if;

							
							if stop_mask_enabled (BOTTOM) then
								set_color_stop_mask (BOTTOM, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
						else -- not flipped
							
							if stop_mask_enabled (TOP) then
								set_color_stop_mask (TOP, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
							if stop_mask_enabled (BOTTOM) then
								set_color_stop_mask (BOTTOM, brightness);

								if f = BOTTOM then
									do_it;
								end if;								
							end if;
							
						end if;
					end draw_stopmask;


					


					-- Draws the stencil (or solder paste mask) of the pad:					
					procedure draw_stencil is 
						
						stencil_contours : type_stencil_contours;
						-- CS initialize (see build_contour)
						

						-- Builds the stencil contour from the pad contour:
						procedure build_contour is 
							use pac_geometry_2;	
							use et_board_geometry.pac_contours;
							use et_board_geometry.pac_polygons;
							use pac_offsetting;

							polygon_tmp : type_polygon;
						begin
							case stencil.shrink_mode is
								
								when AS_PAD =>
									-- Copy pad contours to stencil without
									-- any modification:
									stencil_contours := (type_contour (pad_contours) with null record);

									
								when SHRINK_PAD =>
									-- Copy pad contours to stencil:
									stencil_contours := (type_contour (pad_contours) with null record);

									-- Now the stencil must be shrinked according to shrink_factor:
									
									-- Make a temporary polygon from the stencil contour
									polygon_tmp := to_polygon (stencil_contours, fill_tolerance, EXPAND);
									-- CS: expand correct ?
									
									--scale_polygon (
										--polygon		=> stencil_contours,
										--scale		=> stencil.shrink_factor);

									-- Offset the temporary polygon
									offset_polygon (
										polygon			=> polygon_tmp,
										offset			=> type_float_model (stencil.shrink_factor),
										log_threshold	=> log_threshold + 80); -- CS

									-- Convert the temporary polygon back to a contour:
									stencil_contours := (to_contour (polygon_tmp) with null record);
									
									
								when USER_SPECIFIC =>
									-- Set the stencil contour as given by the user settings:
									stencil_contours := stencil.contour;
									
							end case;
						end build_contour;
						

						
						-- Does the actual drawing of the contour:
						procedure do_it is begin
							draw_contour (
								contour	=> stencil_contours,
								pos		=> get_position (package_position),
								offset	=> pad_position,
								filled	=> YES,
								mirror	=> mirror,
								width	=> zero);
						end do_it;

						
					begin
						build_contour;				

						
						if flip then
							if stencil_enabled (TOP) then
								set_color_stencil (TOP, brightness);

								if f = BOTTOM then
									do_it;
								end if;
							end if;

							
							if stencil_enabled (BOTTOM) then
								set_color_stencil (BOTTOM, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
						else -- not flipped
							
							if stencil_enabled (TOP) then
								set_color_stencil (TOP, brightness);

								if f = TOP then
									do_it;
								end if;								
							end if;

							
							if stencil_enabled (BOTTOM) then
								set_color_stencil (BOTTOM, brightness);

								if f = BOTTOM then
									do_it;
								end if;								
							end if;
							
						end if;				
					end draw_stencil;
					
					
				begin
					draw_conductor;
					draw_stopmask;
					draw_stencil;
				end draw_pad_smt;


				
				
			begin -- query_terminal

				-- The terminal can be a through-hole technology type (THT) 
				-- or a terminal for surface mounting technology (SMT):
				case t.technology is
					
					when THT =>

						-- The pad can have a circular hole or a 
						-- hole of arbitrary shape:
						case t.tht_hole is

							when DRILLED => -- circlular hole

								-- Set the color for conductors
								-- of outer an inner layers:
								set_color_tht_pad (brightness);

								draw_tht_outer_layers_drilled;
								draw_tht_inner_layers_drilled;

								draw_tht_stop_mask;

								-- Draw the name of the terminal:
								draw_name_tht (t.position);

									
							when MILLED => -- arbitrary shape of so called "plated millings"

								-- Set the color for conductors
								-- of outer an inner layers:
								set_color_tht_pad (brightness);

								draw_tht_outer_layers_milled;
								draw_tht_inner_layers_milled;
								
								draw_tht_stop_mask;

								-- Draw the name of the terminal:
								draw_name_tht (t.position);
						end case;

						
					when SMT =>

						draw_pad_smt (t.pad_shape_smt, t.stop_mask_shape_smt,
							t.stencil_shape, t.position, t.face);
				end case;
				
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
		-- The cursor to the actual device model:
		use et_device_library;
		device_model_cursor : pac_device_models.cursor;

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
			
			-- Get the cursor to the device model:
			device_model_cursor := get_device_model (device.model);

			-- Get the name of the package model:
			package_model_name := get_package_model (device_model_cursor, device.variant);

			-- Send the actual package model to the draw procedure:
			draw_package (
				packge	=> pac_package_models.element (package_library, package_model_name));

		end if;
	end query_electrical_device;




	
	-- This procedure draws the package of a
	-- non-electrical device:
	procedure query_non_electrical_device (
		name	: in type_device_name;
		device	: in type_device_non_electrical)
	is begin
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
		draw_package (
			packge	=> pac_package_models.element (package_library, device.package_model));
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
