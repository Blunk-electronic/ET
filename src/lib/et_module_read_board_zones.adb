------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD ZONES NON-ELECTRICAL              --
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
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_board;				use et_module_board;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;			use et_primitive_objects;
with et_via_restrict.boards;
with et_route_restrict.boards;

with et_assy_doc;
with et_silkscreen;
with et_stopmask;
with et_keepout;
with et_stencil;

with et_module_read_board_contour;	use et_module_read_board_contour;

with et_general_rw;					use et_general_rw;



package body et_module_read_board_zones is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;
	use pac_signal_layers;
	
	
	
	board_filled : type_filled := filled_default;
	-- CS rename to zone_filled
	
	fill_spacing : type_track_clearance := type_track_clearance'first;
	-- CS rename to zone_fill_spacing
	
	board_fill_style : type_fill_style := fill_style_default;	
	-- CS rename to zone_fill_style
	
	board_easing : type_easing;
	-- CS rename to zone_easing
	
	signal_layer : type_signal_layer;
	-- CS rename to zone_signal_layer
	
	contour_priority : type_priority := type_priority'first;
	-- CS rename to zone_priority
	
	polygon_width_min : type_track_width := type_track_width'first;
	-- CS rename to zone_width_min
	
	polygon_isolation : type_track_clearance := type_track_clearance'first; 
	-- CS rename to zone_isolation
	-- applies to conductor zones only
		
	signal_layers : pac_signal_layers.set;
	-- CS rename to zone_signal_layers
	

	


	
	
	procedure reset_scratch is begin
		fill_spacing		:= type_track_clearance'first;
		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		--board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		contour_priority		:= type_priority'first;  -- board relevant only
		polygon_isolation		:= type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only
		clear (signal_layers);
		
		reset_contour (contour);
	end;


	

	

	
	
	procedure read_fill_zone_keepout (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_filled then -- filled yes/no
			expect_field_count (line, 2);													
			board_filled := to_filled (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_keepout;


	

	procedure read_keepout_cutout (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
	begin
		-- CS
		-- no parameters for this kind of zone allowed here
		invalid_keyword (kw);
	end read_keepout_cutout;
	


	
	

	procedure read_cutout_non_conductor (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_cutout_non_conductor;


	
	
	
	
	
	procedure read_cutout_conductor_non_electric (
		line : in type_fields_of_line)
	is
		use et_pcb_stack;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		elsif kw = keyword_layer then -- layer 1
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_cutout_conductor_non_electric;

		
	
	
	
	
			
	procedure read_fill_zone_non_conductor (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_fill_style then -- fill_style solid/hatched
			expect_field_count (line, 2);													
			board_fill_style := to_fill_style (f (line, 2));
		
		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_non_conductor;
		
			
			
	
	
	
	
			
	procedure read_fill_zone_conductor_non_electric (
		line : in type_fields_of_line)
	is
		use et_fill_zones.boards;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_fill_style then -- fill_style solid/hatched
			expect_field_count (line, 2);													
			board_fill_style := to_fill_style (f (line, 2));

		elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		elsif kw = keyword_spacing then -- spacing 0.3
			expect_field_count (line, 2);													
			fill_spacing := to_distance (f (line, 2));

		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			polygon_width_min := to_distance (f (line, 2));
			
		elsif kw = keyword_layer then -- layer 1
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));
			
		elsif kw = keyword_priority then -- priority 2
			expect_field_count (line, 2);
			contour_priority := to_priority (f (line, 2));

		elsif kw = keyword_isolation then -- isolation 0.5
			expect_field_count (line, 2);
			polygon_isolation := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_conductor_non_electric;



	
	
	
	procedure read_fill_zone_restrict (
		line	: in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
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
	end read_fill_zone_restrict;

	


	

	
	procedure insert_polygon_route_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is												
		use et_route_restrict.boards;
		use pac_route_restrict_contours;
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			append (
				container	=> module.board.route_restrict.contours,
				new_item	=> (contour with signal_layers));
		end do_it;
							
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_polygon_route_restrict",
			 level => log_threshold);

		log_indentation_up;
		
		-- CS check signal layers
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;
	end insert_polygon_route_restrict;


	

	
	

	procedure insert_zone_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is										   
		use et_via_restrict.boards;
		use pac_via_restrict_contours;
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			append (
				container	=> module.board.via_restrict.contours,
				new_item	=> (contour with signal_layers));
		end do_it;
							
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_zone_via_restrict",
			 level => log_threshold);

		-- CS check signal layers
			 
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;
	end insert_zone_via_restrict;



	


	
	


	procedure insert_polygon_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is									   
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			case board_fill_style is
				when SOLID =>
					pac_floating_solid.append (
						container	=> module.board.conductors_floating.zones.solid,
						new_item	=> (contour with
							fill_style 	=> SOLID,
							easing		=> board_easing,
							islands		=> no_islands,
							properties	=> (signal_layer, contour_priority, others => <>),
							isolation	=> polygon_isolation,
							linewidth	=> polygon_width_min));

				when HATCHED =>
					pac_floating_hatched.append (
						container	=> module.board.conductors_floating.zones.hatched,
						new_item	=> (contour with
							fill_style 	=> HATCHED,
							easing		=> board_easing,
							islands		=> no_islands,
							properties	=> (signal_layer, contour_priority, others => <>),
							isolation	=> polygon_isolation,
							linewidth	=> polygon_width_min,
							spacing		=> fill_spacing));
			end case;
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_polygon_conductor",
			 level => log_threshold);

		-- CS check signal layer
			 
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;
	end insert_polygon_conductor;

				



	


	procedure insert_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
	-- The polygon has been a general thing until now. 
	-- Depending on the layer and the side of the board (face) the polygon
	-- is now assigned to the board where it belongs to.

		use et_stopmask;
		use et_stencil;
		use et_silkscreen;
		use et_assy_doc;
		use et_keepout;
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			
			procedure append_silk_polygon_top is begin
				pac_silk_zones.append (
					container	=> module.board.silkscreen.top.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_silk_polygon_bottom is begin
				pac_silk_zones.append (
					container	=> module.board.silkscreen.bottom.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_assy_doc_polygon_top is begin
				pac_doc_zones.append (
					container	=> module.board.assy_doc.top.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_assy_doc_polygon_bottom is begin
				pac_doc_zones.append (
					container	=> module.board.assy_doc.bottom.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_keepout_polygon_top is begin
				pac_keepout_zones.append (
					container	=> module.board.keepout.top.zones, 
					new_item	=> (contour with null record));
			end;

			
			procedure append_keepout_polygon_bottom is begin
				pac_keepout_zones.append (
					container	=> module.board.keepout.bottom.zones, 
					new_item	=> (contour with null record));
			end;

			
			procedure append_stencil_polygon_top is begin
				pac_stencil_zones.append (
					container	=> module.board.stencil.top.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_stencil_polygon_bottom is begin
				pac_stencil_zones.append (
					container	=> module.board.stencil.bottom.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_stop_polygon_top is begin
				pac_stop_zones.append (
					container	=> module.board.stopmask.top.zones,
					new_item	=> (contour with null record));
			end;

			
			procedure append_stop_polygon_bottom is begin
				pac_stop_zones.append (
					container	=> module.board.stopmask.bottom.zones,
					new_item	=> (contour with null record));
			end;

			
		begin -- do_it
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							append_silk_polygon_top;
										
						when LAYER_CAT_ASSY =>
							append_assy_doc_polygon_top;

						when LAYER_CAT_STENCIL =>
							append_stencil_polygon_top;
							
						when LAYER_CAT_STOPMASK =>
							append_stop_polygon_top;
							
						when LAYER_CAT_KEEPOUT =>
							append_keepout_polygon_top;

						when others => null; -- CS raise exception ?
					end case;
					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							append_silk_polygon_bottom;

						when LAYER_CAT_ASSY =>
							append_assy_doc_polygon_bottom;
							
						when LAYER_CAT_STENCIL =>
							append_stencil_polygon_bottom;
							
						when LAYER_CAT_STOPMASK =>
							append_stop_polygon_bottom;
							
						when LAYER_CAT_KEEPOUT =>
							append_keepout_polygon_bottom;

						when others => null; -- CS raise exception ?
					end case;
					
			end case;
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_polygon",
			 level => log_threshold);

		-- CS log other arguments
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;
	end insert_polygon;

	

	
	
	
	
-- CUTOUT:
	
	
	procedure read_cutout_restrict (
		line	: in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_layers then -- layers 1 14 3

			-- there must be at least two fields:
			expect_field_count (line => line, count_expected => 2, warn => false);
			signal_layers := to_layers (line);

		else
			invalid_keyword (kw);
		end if;
	end read_cutout_restrict;





	

	procedure insert_cutout_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_via_restrict.boards;

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_via_restrict_cutouts.append (
				container	=> module.board.via_restrict.cutouts,
				new_item	=> (contour with
								layers	=> signal_layers));
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_cutout_via_restrict",
			 level => log_threshold);

		log_indentation_up;
		
		-- CS check signal layers
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;
	end insert_cutout_via_restrict;








	
	procedure insert_cutout_route_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_route_restrict.boards;
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			pac_route_restrict_cutouts.append (
				container	=> module.board.route_restrict.cutouts,
				new_item	=> (contour with 
								layers	=> signal_layers));
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_cutout_via_restrict",
			 level => log_threshold);

		log_indentation_up;
		
		-- CS check signal layers
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		reset_scratch;

		log_indentation_down;		
	end insert_cutout_route_restrict;




	
	



	procedure insert_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use et_stopmask;
			use et_keepout;
			
			
			procedure append_keepout_cutout_top is begin
				pac_keepout_cutouts.append (
					container	=> module.board.keepout.top.cutouts, 
					new_item	=> (contour with null record));
			end;

			
			procedure append_keepout_cutout_bottom is begin
				pac_keepout_cutouts.append (
					container	=> module.board.keepout.bottom.cutouts, 
					new_item	=> (contour with null record));
			end;

			
		begin -- do_it
			-- log (text => to_string (contour), level => log_threshold + 1);
		
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_KEEPOUT =>
							append_keepout_cutout_top;

						when others => null; -- CS raise exception ?
						-- CS keepout, stencil, assy, silkscreen ?
						
					end case;
					
				when BOTTOM => null;
					case layer_cat is
						when LAYER_CAT_KEEPOUT =>
							append_keepout_cutout_bottom;

						when others => null; -- CS raise exception ?									
						-- CS keepout, stencil, assy, silkscreen ?
					end case;							
			end case;
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_cutout "
			 & " CAT: " & to_string (layer_cat)
			 & " face: " & to_string (face),
			 level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board cutout
		reset_scratch;

		log_indentation_down;
	end insert_cutout;






	

	procedure insert_cutout_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_cutouts.append (
				container	=> module.board.conductors_floating.cutouts,
				new_item	=> (contour with
						layer => signal_layer));
		end do_it;
							
	begin
		log (text => "module " & to_string (module_cursor)
			 & " insert_cutout",
			 level => log_threshold);

		-- CS log arguments
		-- CS check signal layer
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next floating board contour
		reset_scratch;

		log_indentation_down;
	end insert_cutout_conductor;

	
	
	
end et_module_read_board_zones;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
