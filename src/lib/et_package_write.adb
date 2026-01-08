------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE WRITE                                 --
--                                                                          --
--                               B o d y                                    --
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

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.text_io;				use ada.text_io;
with ada.exceptions;

with et_directory_and_file_ops;
with et_general_rw;						use et_general_rw;

with et_text_content;					use et_text_content;

with et_alignment;						use et_alignment;

with et_terminal_stopmask;				use et_terminal_stopmask;
with et_terminal_stencil;				use et_terminal_stencil;
with et_stopmask_status;				use et_stopmask_status;
with et_stencil_mask_status;			use et_stencil_mask_status;

with et_assembly_technology;			use et_assembly_technology;
with et_terminal_hole;					use et_terminal_hole;
with et_terminals;						use et_terminals;

with et_primitive_objects;				use et_primitive_objects;
with et_conductor_text.packages;		use et_conductor_text.packages;
with et_time;							use et_time;
with et_mirroring;						use et_mirroring;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_system_info;
with et_package_description;			use et_package_description;
with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_board_holes;
with et_conductor_segment;
with et_package_sections;				use et_package_sections;


package body et_package_write is

	use pac_text_board_vectorized;
	use pac_texts_fab_with_content;
	
	use pac_conductor_texts;


	

	procedure write_package (
		file_name 		: in pac_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac							   
		packge			: in type_package_model; -- the actual package model
		log_threshold	: in type_log_level) 
	is
		file_handle : ada.text_io.file_type;

		
		procedure write_text (cursor : in pac_conductor_texts.cursor) is begin
			text_begin;
			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));
			-- CS write_text_properties (element (cursor));
			text_end;
		end write_text;

		
		-- This is about conductor objects in either top or bottom.
		-- These objects have no connection to any pad or signal.
		procedure write_conductor is

			use et_conductor_segment;
			
			use pac_conductor_lines;
			procedure write_line (cursor : in pac_conductor_lines.cursor) is begin
				line_begin;
				write_line (element (cursor));
				write_width (element (cursor).width);
				line_end;
			end write_line;

			
			use pac_conductor_arcs;
			procedure write_arc (cursor : in pac_conductor_arcs.cursor) is begin
				arc_begin;
				write_arc (element (cursor));
				write_width (element (cursor).width);
				arc_end;
			end write_arc;

			
			use pac_conductor_circles;
			procedure write_circle (cursor : in pac_conductor_circles.cursor) is begin
				write_circle_conductor (element (cursor));
			end write_circle;
	
			
		begin -- write_conductor
			section_mark (section_conductor, HEADER);

			-- top
			section_mark (section_top, HEADER);			
			iterate (packge.conductors.top.lines, write_line'access);
			iterate (packge.conductors.top.arcs, write_arc'access);
			iterate (packge.conductors.top.circles, write_circle'access);
			iterate (packge.conductors.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);			
			iterate (packge.conductors.bottom.lines, write_line'access);
			iterate (packge.conductors.bottom.arcs, write_arc'access);
			iterate (packge.conductors.bottom.circles, write_circle'access);
			iterate (packge.conductors.bottom.texts, write_text'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_conductor, FOOTER);
		end write_conductor;

		
		use pac_text_placeholders;		
		procedure write_placeholder (cursor : in pac_text_placeholders.cursor) is begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;

		
		procedure write_silk_screen is 
			use pac_silk_lines;
			use pac_silk_arcs;
			use pac_silk_circles;
			use pac_silk_zones;
			use pac_silk_texts;

			-- CS move this procedure to et_pcb_rw
			procedure write_text (cursor : in pac_silk_texts.cursor) is begin
				text_begin;
				write (keyword => keyword_content, wrap => true,
					parameters => to_string (element (cursor).content));

				write_text_properties (element (cursor));
				text_end;
			end write_text;

		begin
			section_mark (section_silkscreen, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.silkscreen.top.lines, write_line'access);
			iterate (packge.silkscreen.top.arcs, write_arc'access);
			iterate (packge.silkscreen.top.circles, write_circle'access);
			iterate (packge.silkscreen.top.zones, write_polygon'access);
			iterate (packge.silkscreen.top.texts, write_text'access);
			iterate (packge.silkscreen.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.silkscreen.bottom.lines, write_line'access);
			iterate (packge.silkscreen.bottom.arcs, write_arc'access);
			iterate (packge.silkscreen.bottom.circles, write_circle'access);
			iterate (packge.silkscreen.bottom.zones, write_polygon'access);
			iterate (packge.silkscreen.bottom.texts, write_text'access);
			iterate (packge.silkscreen.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_silkscreen, FOOTER);			
		end write_silk_screen;

		
		procedure write_assembly_documentation is 
			use pac_doc_lines;
			use pac_doc_arcs;
			use pac_doc_circles;
			use pac_doc_zones;
			use pac_doc_texts;

			-- CS move this procedure to et_pcb_rw
			procedure write_text (cursor : in pac_doc_texts.cursor) is begin
				text_begin;
				write (keyword => keyword_content, wrap => true,
					parameters => to_string (element (cursor).content));

				write_text_properties (element (cursor));
				text_end;
			end write_text;

		begin
			section_mark (section_assembly_doc, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.assy_doc.top.lines, write_line'access);
			iterate (packge.assy_doc.top.arcs, write_arc'access);
			iterate (packge.assy_doc.top.circles, write_circle'access);
			iterate (packge.assy_doc.top.zones, write_polygon'access);
			iterate (packge.assy_doc.top.texts, write_text'access);
			iterate (packge.assy_doc.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.assy_doc.bottom.lines, write_line'access);
			iterate (packge.assy_doc.bottom.arcs, write_arc'access);
			iterate (packge.assy_doc.bottom.circles, write_circle'access);
			iterate (packge.assy_doc.bottom.zones, write_polygon'access);
			iterate (packge.assy_doc.bottom.texts, write_text'access);
			iterate (packge.assy_doc.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_assembly_doc, FOOTER);
		end write_assembly_documentation;

		
		procedure write_keepout is 
			use pac_keepout_zones;
			use pac_keepout_cutouts;
		begin
			section_mark (section_keepout, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.keepout.top.zones, write_polygon'access);
			iterate (packge.keepout.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.keepout.bottom.zones, write_polygon'access);			
			iterate (packge.keepout.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_keepout, FOOTER);			
		end write_keepout;

		
		procedure write_stop_mask is 
			use pac_stop_lines;
			use pac_stop_arcs;
			use pac_stop_circles;
			use pac_stop_zones;
		begin
			section_mark (section_stopmask, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stop_mask.top.lines, write_line'access);
			iterate (packge.stop_mask.top.arcs, write_arc'access);
			iterate (packge.stop_mask.top.circles, write_circle'access);
			iterate (packge.stop_mask.top.zones, write_polygon'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stop_mask.bottom.lines, write_line'access);
			iterate (packge.stop_mask.bottom.arcs, write_arc'access);
			iterate (packge.stop_mask.bottom.circles, write_circle'access);
			iterate (packge.stop_mask.bottom.zones, write_polygon'access);			
			section_mark (section_bottom, FOOTER);

			section_mark (section_stopmask, FOOTER);			
		end write_stop_mask;

		
		procedure write_stencil is 
			use pac_stencil_lines;
			use pac_stencil_arcs;
			use pac_stencil_circles;
			use pac_stencil_zones;
		begin
			section_mark (section_stencil, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.stencil.top.lines, write_line'access);
			iterate (packge.stencil.top.arcs, write_arc'access);
			iterate (packge.stencil.top.circles, write_circle'access);
			iterate (packge.stencil.top.zones, write_polygon'access);
			section_mark (section_top, FOOTER);
			
			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.stencil.bottom.lines, write_line'access);
			iterate (packge.stencil.bottom.arcs, write_arc'access);
			iterate (packge.stencil.bottom.circles, write_circle'access);
			iterate (packge.stencil.bottom.zones, write_polygon'access);
			section_mark (section_bottom, FOOTER);

			section_mark (section_stencil, FOOTER);			
		end write_stencil;

		
		procedure write_route_restrict is 
			use pac_route_restrict_lines;
			use pac_route_restrict_arcs;
			use pac_route_restrict_circles;
			use pac_route_restrict_zones;
			use pac_route_restrict_cutouts;

			procedure write_line (cursor : in pac_route_restrict_lines.cursor) is 
			begin
				line_begin;
				write_line (element (cursor));
				line_end;
			end write_line;

			procedure write_arc (cursor : in pac_route_restrict_arcs.cursor) is 
			begin
				arc_begin;
				write_arc (element (cursor));
				arc_end;
			end write_arc;

			procedure write_circle (cursor : in pac_route_restrict_circles.cursor) is 
			begin
				circle_begin;
				write_circle (element (cursor));
				circle_end;
			end write_circle;
			
			procedure write_zone (cursor : in pac_route_restrict_zones.cursor) is 
			begin
				fill_zone_begin;
				contours_begin;
				write_polygon_segments (element (cursor));
				contours_end;
				fill_zone_end;
			end write_zone;

			procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
			begin
				cutout_zone_begin;
				contours_begin;
				write_polygon_segments (element (cursor));
				contours_end;
				cutout_zone_end;
			end write_cutout;
			
		begin
			section_mark (section_route_restrict, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.route_restrict.top.lines, write_line'access);
			iterate (packge.route_restrict.top.arcs, write_arc'access);
			iterate (packge.route_restrict.top.circles, write_circle'access);
			iterate (packge.route_restrict.top.zones, write_zone'access);
			iterate (packge.route_restrict.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.route_restrict.bottom.lines, write_line'access);
			iterate (packge.route_restrict.bottom.arcs, write_arc'access);
			iterate (packge.route_restrict.bottom.circles, write_circle'access);
			iterate (packge.route_restrict.bottom.zones, write_zone'access);
			iterate (packge.route_restrict.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);
			
			section_mark (section_route_restrict, FOOTER);			
		end write_route_restrict;

		
		procedure write_via_restrict is 
			use pac_via_restrict_zones;
			use pac_via_restrict_cutouts;

			procedure write_zone (cursor : in pac_via_restrict_zones.cursor) is 
			begin
				fill_zone_begin;
				contours_begin;
				write_polygon_segments (element (cursor));
				contours_end;
				fill_zone_end;
			end write_zone;

			procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor) is 
			begin
				cutout_zone_begin;
				contours_begin;
				write_polygon_segments (element (cursor));
				contours_end;
				cutout_zone_end;
			end write_cutout;
			
		begin
			section_mark (section_via_restrict, HEADER);

			-- top
			section_mark (section_top, HEADER);
			iterate (packge.via_restrict.top.zones, write_zone'access);			
			iterate (packge.via_restrict.top.cutouts, write_cutout'access);
			section_mark (section_top, FOOTER);

			-- bottom
			section_mark (section_bottom, HEADER);
			iterate (packge.via_restrict.bottom.zones, write_zone'access);			
			iterate (packge.via_restrict.bottom.cutouts, write_cutout'access);
			section_mark (section_bottom, FOOTER);
			
			section_mark (section_via_restrict, FOOTER);			
		end write_via_restrict;

		
		procedure write_holes is
			use et_board_holes;
			use pac_holes;
			use pac_segments;

			procedure query_hole (c : in pac_holes.cursor) is begin
				section_mark (section_hole, HEADER);		
				write_polygon_segments (element (c));		
				section_mark (section_hole, FOOTER);		
			end query_hole;

		begin
			if not is_empty (packge.holes) then
				
				section_mark (section_pcb_contours, HEADER);		
				packge.holes.iterate (query_hole'access);				
				section_mark (section_pcb_contours, FOOTER);
				
			end if;
		end write_holes;


		procedure write_package_contour is begin
			section_mark (section_pac_3d_contours, HEADER);
			-- CS
			section_mark (section_pac_3d_contours, FOOTER);
		end write_package_contour;

		
		procedure write_terminals is
			use pac_terminals;
			terminal_cursor : pac_terminals.cursor := packge.terminals.first;

			procedure write_stop_mask_tht is 
				
				function user_specific_contours return boolean is begin
					if element (terminal_cursor).stop_mask_shape_tht.top.expand_mode = USER_SPECIFIC 
					or element (terminal_cursor).stop_mask_shape_tht.bottom.expand_mode = USER_SPECIFIC then
						return true;
					else
						return false;
					end if;
				end user_specific_contours;
				
			begin -- write_stop_mask_tht
				write (keyword => keyword_stop_mask_status, 
					   parameters => to_string (element (terminal_cursor).stop_mask_status_tht)); -- stop_mask_status open
					   
				write (keyword => keyword_stop_mask_shape_top, 
						parameters => to_string (element (terminal_cursor).stop_mask_shape_tht.top.expand_mode));

				write (keyword => keyword_stop_mask_shape_bottom, 
						parameters => to_string (element (terminal_cursor).stop_mask_shape_tht.bottom.expand_mode));

				-- If user specified contours in either top or bottom required, write the header
				-- for stop mask contours:
				if user_specific_contours then
					section_mark (section_stopmask_contours_tht, HEADER);
				end if;

				-- If user specified contours in top, write them:
				case element (terminal_cursor).stop_mask_shape_tht.top.expand_mode is
					when AS_PAD | EXPAND_PAD => null;
					when USER_SPECIFIC =>
						section_mark (section_top, HEADER);
						
						write_polygon_segments (type_contour (
							element (terminal_cursor).stop_mask_shape_tht.top.contour));

						section_mark (section_top, FOOTER);
				end case;

				-- If user specified contours in bottom, write them:
				case element (terminal_cursor).stop_mask_shape_tht.bottom.expand_mode is
					when AS_PAD | EXPAND_PAD => null;
					when USER_SPECIFIC =>
						section_mark (section_bottom, HEADER);
						
						write_polygon_segments (type_contour (
							element (terminal_cursor).stop_mask_shape_tht.bottom.contour));

						section_mark (section_bottom, FOOTER);
				end case;
				
				-- If user specified contours in either top or bottom required, write the footer
				-- for stop mask contours:
				if user_specific_contours then
					section_mark (section_stopmask_contours_tht, FOOTER);
				end if;
				
			end write_stop_mask_tht;

			
			procedure write_stop_mask_smt is 
				
				function user_specific_contours return boolean is begin
					if element (terminal_cursor).stop_mask_shape_smt.expand_mode = USER_SPECIFIC then
						return true;
					else
						return false;
					end if;
				end user_specific_contours;
				
			begin -- write_stop_mask_smt
				write (keyword => keyword_stop_mask_status, 
					   parameters => to_string (element (terminal_cursor).stop_mask_status_smt)); -- stop_mask_status open
				
				write (keyword => keyword_stop_mask_shape, 
						parameters => to_string (element (terminal_cursor).stop_mask_shape_smt.expand_mode)); -- stop_mask_shape as_pad/expand_pad/user_specific

				-- If user specified contours required, write the header for stop mask contours:
				if user_specific_contours then
					section_mark (section_stopmask_contours_smt, HEADER);
				end if;

				-- If user specified contours, write them:
				case element (terminal_cursor).stop_mask_shape_smt.expand_mode is
					when AS_PAD | EXPAND_PAD => null;
					when USER_SPECIFIC =>
		
						write_polygon_segments (type_contour (
							element (terminal_cursor).stop_mask_shape_smt.contour));

				end case;

				-- If user specified contours required, write the footer for stop mask contours:
				if user_specific_contours then
					section_mark (section_stopmask_contours_smt, FOOTER);
				end if;
				
			end write_stop_mask_smt;

			
			procedure write_plated_millings (
				millings : in type_contour) 
			is begin
				section_mark (section_pad_millings, HEADER);
				write_polygon_segments (type_contour (millings));
				section_mark (section_pad_millings, FOOTER);
			end write_plated_millings;

			
			procedure write_stencil is
				
				function user_specific_contours return boolean is begin
					if element (terminal_cursor).stencil_shape.shrink_mode = USER_SPECIFIC then
						return true;
					else
						return false;
					end if;
				end user_specific_contours;

			begin
				write (keyword => keyword_solder_paste_status,
					   parameters => to_string (element (terminal_cursor).solder_paste_status)); 
					-- solder_paste_status applied
				
				write (keyword => keyword_solder_paste_shape,
					   parameters => to_string (element (terminal_cursor).stencil_shape.shrink_mode)); 
					-- solder_paste_shape as_pad/shrink_pad/user_specific

				-- If user specified contours required, write the header for stencil contours:
				if user_specific_contours then
					section_mark (section_stencil_contours, HEADER);
				end if;
				
				case element (terminal_cursor).stencil_shape.shrink_mode is
					when AS_PAD => null;
					
					when SHRINK_PAD	=>
						write (
							keyword		=> keyword_solder_paste_shrink_factor,
							parameters	=> to_string (element (terminal_cursor).stencil_shape.shrink_factor));
							-- solder_paste_shrink_factor 0.4

					when USER_SPECIFIC =>

						write_polygon_segments (type_contour (
							element (terminal_cursor).stencil_shape.contour));
				end case;

				-- If user specified contours required, write the footer for stencil contours:
				if user_specific_contours then
					section_mark (section_stencil_contours, FOOTER);
				end if;
				
			end write_stencil;

			
		begin -- write_terminals
			section_mark (section_terminals, HEADER);
			
			while terminal_cursor /= pac_terminals.no_element loop
				section_mark (section_terminal, HEADER);
				write (keyword => keyword_name, parameters => space & to_string (key (terminal_cursor)));
				write (keyword => keyword_assembly_technology, parameters => to_string (element (terminal_cursor).technology));
				write (keyword => keyword_position, parameters => to_string (element (terminal_cursor).position));
				
				case element (terminal_cursor).technology is
					when THT =>
						-- pad contour top
						section_mark (section_pad_contours_tht, HEADER);
						
						section_mark (section_top, HEADER);
						write_polygon_segments (type_contour (element (terminal_cursor).pad_shape_tht.top));
						section_mark (section_top, FOOTER);

						-- pad contour bottom
						section_mark (section_bottom, HEADER);
						write_polygon_segments (type_contour (element (terminal_cursor).pad_shape_tht.bottom));
						section_mark (section_bottom, FOOTER);
						
						section_mark (section_pad_contours_tht, FOOTER);

						-- stop mask
						write_stop_mask_tht;
						
						-- conductor width in inner layers
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
						write_polygon_segments (type_contour (element (terminal_cursor).pad_shape_smt));
						section_mark (section_pad_contours_smt, FOOTER);
						
						write (keyword => keyword_face, 
							   parameters => to_string (element (terminal_cursor).face));

						-- stop mask
						write_stop_mask_smt;

						-- solder paste / stencil
						write_stencil;
				end case;

				section_mark (section_terminal, FOOTER);
				next (terminal_cursor);
			end loop;
			
			section_mark (section_terminals, FOOTER);
		end write_terminals;

		
	begin -- save_package
		log (text => to_string (file_name), level => log_threshold);

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);
		
		-- write a nice header
		put_line (comment_mark & " " & et_system_info.system_name & " package");
		put_line (comment_mark & " " & get_date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;

		reset_tab_depth;

		write (keyword => keyword_description, wrap => true, 
			   parameters => to_string (packge.description));

		write (keyword => keyword_bom_relevant, parameters => to_string (packge.appearance));
		write (keyword => keyword_assembly_technology, parameters => to_string (packge.technology));

		write_silk_screen;
		write_assembly_documentation;
		write_keepout;
		write_conductor;
		write_stop_mask;
		write_stencil;
		write_route_restrict;
		write_via_restrict;
		write_holes; -- pcb cutouts
		-- write_contour_plated; -- pcb contour -- CS currently no need
		write_terminals; -- incl. pad properties, drill sizes, millings, ...

		-- 3D stuff
		if packge.appearance = BOM_RELEVANT_YES then
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

	end write_package;
	
		
	
end et_package_write;
