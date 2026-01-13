------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       PACKAGE WRITE / TERMINALS                          --
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

-- To Do:
-- - clean up, use renames


with ada.text_io;				use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;

with et_keywords;						use et_keywords;
with et_package_sections;				use et_package_sections;

with et_pcb_sides;						use et_pcb_sides;

with et_assembly_technology;			use et_assembly_technology;
with et_terminal_stopmask;				use et_terminal_stopmask;
with et_terminal_stencil;				use et_terminal_stencil;
with et_stopmask_status;				use et_stopmask_status;
with et_stencil_mask_status;			use et_stencil_mask_status;

with et_terminal_hole;					use et_terminal_hole;
with et_terminal_name;					use et_terminal_name;
with et_terminals;						use et_terminals;

with et_board_geometry;					use et_board_geometry;
with et_file_write;						use et_file_write;



package body et_package_write_terminals is

	use pac_geometry_2;
	use pac_file_rw;
	use pac_contours;
	
	use pac_terminals;
	

	procedure write_terminals (
		packge			: in type_package_model;
		log_threshold	: in type_log_level)
	is 
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



		
	begin
		section_mark (section_terminals, HEADER);
		
		while terminal_cursor /= pac_terminals.no_element loop
			section_mark (section_terminal, HEADER);
			write (keyword => keyword_name, parameters => space & to_string (key (terminal_cursor)));
			write (keyword => keyword_assembly_technology, parameters => to_string (element (terminal_cursor).technology));
			write (keyword => keyword_position, parameters => to_string (element (terminal_cursor).position));
			-- CS change format to x 12 y 45 rotation 90
			
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
	

	
	
end et_package_write_terminals;
