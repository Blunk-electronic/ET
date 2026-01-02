------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                 MODULE WRITE / BOARD ZONES NON-ELECTRICAL                --
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
--                                                                          --
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
-- ToDo:
-- - clean up
-- - rework (use renames)
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_module;						use et_module;
with et_module_board;				use et_module_board;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;			use et_primitive_objects;
with et_via_restrict.boards;
with et_route_restrict.boards;
-- with et_conductors_floating_board;

with et_assy_doc;
with et_silkscreen;
with et_stopmask;
with et_keepout;
with et_stencil;

with et_general_rw;					use et_general_rw;
with et_board_write;				use et_board_write;


package body et_module_write_board_zones is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;
	use pac_signal_layers;
	



	procedure write_zones_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_floating_solid;
		use pac_floating_hatched;

		
		procedure write_polygon (cursor : in pac_floating_solid.cursor) is begin
			fill_zone_begin;

			write_easing (element (cursor).easing);

			write_width (element (cursor).linewidth);
			write_isolation (element (cursor).isolation);

			write_priority (element (cursor).properties.priority_level);
			write_signal_layer (element (cursor).properties.layer);

			write_fill_style (element (cursor).fill_style);

			contours_begin;
			write_polygon_segments (type_contour (element (cursor)));
			contours_end;
			
			fill_zone_end;
		end;

		
		procedure write_polygon (cursor : in pac_floating_hatched.cursor) is begin
			fill_zone_begin;

			write_easing (element (cursor).easing);

			write_width (element (cursor).linewidth);
			write_isolation (element (cursor).isolation);

			write_priority (element (cursor).properties.priority_level);
			write_signal_layer (element (cursor).properties.layer);

			write_fill_style (element (cursor).fill_style);
			write_spacing (element (cursor).spacing);

			contours_begin;
			write_polygon_segments (type_contour (element (cursor)));
			contours_end;
			
			fill_zone_end;
		end;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			zones : type_floating renames module.board.conductors_floating.zones;
		begin
			iterate (zones.solid, write_polygon'access);
			iterate (zones.hatched, write_polygon'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write floating fill zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_zones_conductor;
	
	
	

	

	procedure write_zones_conductor_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_cutouts;

		
		procedure write_cutout (cursor : in pac_cutouts.cursor) is begin
			cutout_zone_begin;
			write_signal_layer (element (cursor).layer);
			
			contours_begin; -- CS correct ?
			write_polygon_segments (type_contour (element (cursor)));
			contours_end; -- CS correct ?
			
			cutout_zone_end;
		end;

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			cutouts : pac_cutouts.list renames module.board.conductors_floating.cutouts;
		begin
			iterate (cutouts, write_cutout'access);
		end query_module;

		

	begin
		log (text => "module " & to_string (module_cursor)
			 & " write conductor cutout zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_zones_conductor_cutout;
	






	

	procedure write_zones_non_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use et_silkscreen;
		use pac_silk_zones;

		use et_assy_doc;
		use pac_doc_zones;

		use et_stopmask;
		use pac_stop_zones;

		use et_keepout;
		use pac_keepout_zones;

		use et_stencil;
		use pac_stencil_zones;

		
		procedure write_polygon (cursor : in pac_silk_zones.cursor) is 
		begin
			fill_zone_begin;
			contours_begin;		
			write_polygon_segments (type_contour (element (cursor)));
			contours_end;
			fill_zone_end;
		end write_polygon;


		procedure write_polygon (cursor : in pac_doc_zones.cursor) is 
		begin
			fill_zone_begin;
			contours_begin;		
			write_polygon_segments (element (cursor));
			contours_end;
			fill_zone_end;
		end write_polygon;


		procedure write_polygon (cursor : in pac_keepout_zones.cursor) is 		
		begin
			fill_zone_begin;
			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			fill_zone_end;
		end write_polygon;


		procedure write_polygon (cursor : in pac_stop_zones.cursor) is 
		begin
			fill_zone_begin;
			contours_begin;		
			write_polygon_segments (element (cursor));
			contours_end;
			fill_zone_end;
		end write_polygon;


		procedure write_polygon (cursor : in pac_stencil_zones.cursor) is 
		begin
			fill_zone_begin;
			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			fill_zone_end;
		end write_polygon;

		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 

		begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.top.zones, write_polygon'access);
										
						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.top.zones, write_polygon'access);

						when LAYER_CAT_STENCIL =>
							iterate (module.board.stencil.top.zones, write_polygon'access);
							
						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.top.zones, write_polygon'access);
							
						when LAYER_CAT_KEEPOUT =>
							iterate (module.board.keepout.top.zones, write_polygon'access);

						when others => null; -- CS raise exception ?
					end case;
					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.bottom.zones, write_polygon'access);

						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.bottom.zones, write_polygon'access);
							
						when LAYER_CAT_STENCIL =>
							iterate (module.board.stencil.bottom.zones, write_polygon'access);
							
						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.bottom.zones, write_polygon'access);
							
						when LAYER_CAT_KEEPOUT =>
							iterate (module.board.keepout.bottom.zones, write_polygon'access);

						when others => null; -- CS raise exception ?
					end case;
					
			end case;

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write non-conductor zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_zones_non_conductor;

	





	procedure write_zones_non_conductor_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		use et_keepout;
		use pac_keepout_cutouts;


		procedure write_cutout (cursor : in pac_keepout_cutouts.cursor) is 
		begin
			cutout_zone_begin;		
			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			cutout_zone_end;
		end;

		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							null; -- CS
										
						when LAYER_CAT_ASSY =>
							null; -- CS

						when LAYER_CAT_STENCIL =>
							null; -- CS
							
						when LAYER_CAT_STOPMASK =>
							null; -- CS
							
						when LAYER_CAT_KEEPOUT =>
							iterate (module.board.keepout.top.cutouts, write_cutout'access);

						when others => null; -- CS raise exception ?
					end case;

					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							null; -- CS
										
						when LAYER_CAT_ASSY =>
							null; -- CS

						when LAYER_CAT_STENCIL =>
							null; -- CS
							
						when LAYER_CAT_STOPMASK =>
							null; -- CS
							
						when LAYER_CAT_KEEPOUT =>
							iterate (module.board.keepout.bottom.cutouts, write_cutout'access);

						when others => null; -- CS raise exception ?
					end case;					
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write non-conductor zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_zones_non_conductor_cutout;








	

	procedure write_zones_route_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_route_restrict.boards;
		use pac_route_restrict_contours;
		

		procedure write_contour (cursor : in pac_route_restrict_contours.cursor) is 
		begin
			fill_zone_begin;
			write_signal_layers (element (cursor).layers);

			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			
			fill_zone_end;
		end write_contour;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			iterate (module.board.route_restrict.contours, write_contour'access);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write route restrict zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_zones_route_restrict;

	


	


	procedure write_zones_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_via_restrict.boards;
		use pac_via_restrict_contours;
		

		procedure write_contour (cursor : in pac_via_restrict_contours.cursor) is 
		begin
			fill_zone_begin;
			write_signal_layers (element (cursor).layers);

			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			
			fill_zone_end;
		end write_contour;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			iterate (module.board.via_restrict.contours, write_contour'access);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write via restrict zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
	end write_zones_via_restrict;
	




	

	procedure write_zones_route_restrict_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 
		use et_route_restrict.boards;
		use pac_route_restrict_cutouts;

		
		procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
		begin
			cutout_zone_begin;
			write_signal_layers (element (cursor).layers);

			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			
			cutout_zone_end;
		end;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			iterate (module.board.route_restrict.cutouts, write_cutout'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write route restrict cutout zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_zones_route_restrict_cutout;



	

	

	procedure write_zones_via_restrict_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_via_restrict.boards;
		use pac_via_restrict_cutouts;

		
		procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor) is 
		begin
			cutout_zone_begin;
			write_signal_layers (element (cursor).layers);
			
			contours_begin;
			write_polygon_segments (element (cursor));
			contours_end;
			
			cutout_zone_end;
		end;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			iterate (module.board.via_restrict.cutouts, write_cutout'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write via restrict cutout zones",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_zones_via_restrict_cutout;

	
	
end et_module_write_board_zones;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
