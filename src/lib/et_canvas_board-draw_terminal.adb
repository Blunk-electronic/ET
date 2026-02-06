------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW TERMINAL                             --
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

with et_assembly_technology;		use et_assembly_technology;
with et_terminals;					use et_terminals;
with et_terminal_name;				use et_terminal_name;
with et_terminal_hole;				use et_terminal_hole;
with et_terminal_tht;				use et_terminal_tht;

with et_pcb_signal_layers;			use et_pcb_signal_layers;

with et_board_text;

with et_display.board;				use et_display.board;
with et_colors;						use et_colors;
with et_colors.board;				use et_colors.board;
with et_design_rules_board;			use et_design_rules_board;

with et_text_content;				use et_text_content;
with et_alignment;


with et_terminal_stencil;			use et_terminal_stencil;
with et_terminal_stopmask;			use et_terminal_stopmask;

with et_contour_to_polygon;			use et_contour_to_polygon;

with et_board_ops_signal_layers;	use et_board_ops_signal_layers;



separate (et_canvas_board)


-- Draws a single terminal candidate:
procedure draw_terminal (
	name				: in et_terminal_name.pac_terminal_name.bounded_string;
	terminal			: in et_terminals.type_terminal;
	brightness			: in et_colors.type_brightness;
	package_position	: in type_package_position;
	mirror				: in et_mirroring.type_mirror;
	flip				: in boolean)
is
	t : type_terminal renames terminal;

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
					circle	=> circle, 
					pos		=> get_position (package_position), 
					filled	=> NO,
					width	=> restring,
					mirror	=> mirror,
					stroke	=> DO_STROKE);


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
			
			-- Draw the terminal name at pos_tmp:							
			draw_text (
				content		=> to_content (to_string (name)),
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

		-- Draw the terminal name at pos_tmp:
		draw_text (
			content		=> to_content (to_string (name)),
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
	
end draw_terminal;




-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
