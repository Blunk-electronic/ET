------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE READ / TERMINAL                           --
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
--
-- DESCRIPTION:
-- 
-- This is about contours in general.
--
--
--
-- To Do:
-- - clean up
--

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;

with et_keywords;						use et_keywords;

with et_general_rw;						use et_general_rw;

with et_package_read_contour;			use et_package_read_contour;



package body et_package_read_terminal is


	procedure read_terminal  (
		line : in type_fields_of_line)	
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name 1,2,H7
			expect_field_count (line, 2);
			terminal_name := to_terminal_name (f (line,2));

		elsif kw = keyword_assembly_technology then -- technology tht
			expect_field_count (line, 2);
			terminal_technology := to_assembly_technology (f (line,2));

		elsif kw = keyword_position then -- position x 12.7 y 3.0 rotation 0.0
			expect_field_count (line, 7);
			terminal_position := to_position (line,2);

		elsif kw = keyword_width_inner_layers then -- width_inner_layers 0.2
			expect_field_count (line, 2);
			tht_width_inner_layers := to_distance (f (line,2));

		elsif kw = keyword_tht_hole then -- hole drilled/milled
			expect_field_count (line, 2);
			tht_hole := to_tht_hole (f (line,2));

		elsif kw = keyword_drill_size then -- drill_size 0.8
			expect_field_count (line, 2);
			tht_drill_size := to_distance (f (line,2));
			
		elsif kw = keyword_face then -- face top/bottom
			expect_field_count (line, 2);
			smt_pad_face := to_face (f (line,2));

		elsif kw = keyword_stop_mask_status then -- stop_mask_status open/closed
			expect_field_count (line, 2);
			smt_stop_mask_status := to_stop_mask_status (f (line,2));

		elsif kw = keyword_stop_mask_shape then -- keyword_stop_mask_shape user_specific
			expect_field_count (line, 2);
			smt_stop_mask_shape := to_shape (f (line,2));
			
		elsif kw = keyword_stop_mask_shape_top then -- stop_mask_shape_top user_specific
			expect_field_count (line, 2);
			tht_stop_mask_shape_top := to_shape (f (line,2));

		elsif kw = keyword_stop_mask_shape_bottom then -- keyword_stop_mask_shape_bottom user_specific
			expect_field_count (line, 2);
			tht_stop_mask_shape_bottom := to_shape (f (line,2));

		elsif kw = keyword_solder_paste_status then -- solder_paste_status applied/none
			expect_field_count (line, 2);
			smt_solder_paste_status := to_solder_paste_status (f (line,2));

		elsif kw = keyword_solder_paste_shape then -- solder_paste_shape as_pad/shrink_pad/user_specific
			expect_field_count (line, 2);
			smt_stencil_shape := to_modification (f (line,2));

		elsif kw = keyword_solder_paste_shrink_factor then -- solder_paste_shrink_factor 0.5
			expect_field_count (line, 2);
			--smt_stencil_shrink := to_scale (f (line,2));
			smt_stencil_shrink := to_distance (f (line,2));
			
		else
			invalid_keyword (kw);
		end if;

	end read_terminal;






	procedure assign_contour_conductor_tht  (
		face : in type_face)
	is begin
		-- CS check_outline (contour, log_threshold + 1);

		case face is
			when TOP =>
				tht_pad_shape.top := contour;

			when BOTTOM =>
				tht_pad_shape.bottom := contour;
		end case;
		
		reset_contour (contour);
	end;



	procedure assign_contour_stopmask_tht (
		face : in type_face)
	is begin
		-- CS check_outline (contour, log_threshold + 1);

		case face is
			when TOP =>
				tht_stop_mask_contours_top := (contour with null record);

			when BOTTOM =>
				tht_stop_mask_contours_bottom := (contour with null record);
		end case;
		
		reset_contour (contour);
	end;

	

	procedure assign_plated_millings is begin
		-- CS check_outline (contour, log_threshold + 1);
		tht_millings := contour;

		reset_contour (contour);
	end;
	

	


	procedure assign_contour_conductor_smt is begin
		-- CS check_outline (contour, log_threshold + 1);
		smt_pad_shape := contour;
		
		reset_contour (contour);
	end;

	
	
	
	procedure assign_contour_stopmask_smt is begin
		-- CS check_outline (contour, log_threshold + 1);
		smt_stop_mask_contours := (contour with null record);

		reset_contour (contour);
	end;

	

	
	procedure assign_contour_stencil_smt is begin
		-- CS check_outline (contour, log_threshold + 1);
		smt_stencil_contours := (contour with null record);

		reset_contour (contour);
	end;

	

	
	
	procedure build_terminal (
		packge			: in type_package_model_access;
		log_threshold	: in type_log_level)
	is 
		cursor : pac_terminals.cursor;
		inserted : boolean;

		-- Builds the stop mask of the terminal if it is a SMT type:
		function make_stop_mask_smt return type_stop_mask_smt is begin
			case smt_stop_mask_shape is
				when AS_PAD =>
					return (
						expand_mode	=> AS_PAD);
					
				when EXPAND_PAD =>
					return (
						expand_mode	=> EXPAND_PAD);
				
				when USER_SPECIFIC =>
					return (
						expand_mode	=> USER_SPECIFIC,
						contour		=> smt_stop_mask_contours);
			end case;
		end make_stop_mask_smt;


		-- Builds the stop mask of the terminal if it is a THT type:
		function make_stop_mask_tht return type_stop_mask_tht is begin
			return r : type_stop_mask_tht do
				case tht_stop_mask_shape_top is
					when AS_PAD => 
						r.top := (expand_mode => AS_PAD);
					
					when EXPAND_PAD =>
						r.top := (expand_mode => EXPAND_PAD);
						
					when USER_SPECIFIC =>
						r.top := (expand_mode => USER_SPECIFIC, 
							contour => tht_stop_mask_contours_top);
				end case;

				case tht_stop_mask_shape_bottom is
					when AS_PAD => 
						r.bottom := (expand_mode => AS_PAD);
						
					when EXPAND_PAD =>
						r.bottom := (expand_mode => EXPAND_PAD);
						
					when USER_SPECIFIC =>
						r.bottom := (expand_mode => USER_SPECIFIC, 
							contour => tht_stop_mask_contours_bottom);

				end case;
			end return;
		end make_stop_mask_tht;

		
		-- Builds the stencil of the SMT pad (there is no stencil for THT pads):
		function make_stencil return type_stencil_shape is begin
			return r : type_stencil_shape do
				case smt_stencil_shape is
					when AS_PAD =>
						r := (shape => AS_PAD);
					when SHRINK_PAD =>
						r := (shape => SHRINK_PAD, shrink_factor => smt_stencil_shrink);
					when USER_SPECIFIC =>
						r := (shape => USER_SPECIFIC, contours => smt_stencil_contours);
				end case;
			end return;
		end make_stencil;

		
	begin -- build_terminal
		case terminal_technology is
			when THT => 
				case tht_hole is
					when DRILLED =>

						pac_terminals.insert (
							container	=> packge.terminals,
							key			=> terminal_name, -- 1,4,16
							position	=> cursor,
							inserted	=> inserted,
							new_item	=> (
								technology			=> THT,
								tht_hole			=> DRILLED,
								drill_size			=> tht_drill_size,
								position			=> terminal_position,
								pad_shape_tht		=> tht_pad_shape,
								stop_mask_status_tht	=> tht_stop_mask_status,
								stop_mask_shape_tht		=> make_stop_mask_tht,
								width_inner_layers		=> tht_width_inner_layers));

					when MILLED =>
						pac_terminals.insert (
							container	=> packge.terminals,
							key			=> terminal_name, -- 1,4,16
							position	=> cursor,
							inserted	=> inserted,
							new_item	=> (
								technology			=> THT,
								tht_hole			=> MILLED,
								millings			=> tht_millings,
								position			=> terminal_position,
								pad_shape_tht		=> tht_pad_shape,
								stop_mask_status_tht	=> tht_stop_mask_status,
								stop_mask_shape_tht		=> make_stop_mask_tht,
								width_inner_layers		=> tht_width_inner_layers));
				end case;

				-- clean up for next terminal
				tht_pad_shape			:= (others => <>);
				tht_hole				:= terminal_tht_hole_default;
				tht_width_inner_layers	:= type_track_width'first;
				tht_drill_size			:= type_drill_size_tht'first;

				tht_stop_mask_status			:= stop_mask_status_default;
				tht_stop_mask_shape_top			:= stopmask_expand_mode_default;
				tht_stop_mask_shape_bottom		:= stopmask_expand_mode_default;
				delete_segments (tht_stop_mask_contours_top);
				delete_segments (tht_stop_mask_contours_bottom);
				
			when SMT =>
				pac_terminals.insert (
					container	=> packge.terminals,
					key			=> terminal_name, -- 1,4,16,H9
					position	=> cursor,
					inserted	=> inserted,
					new_item	=> (
						technology				=> SMT,
						tht_hole				=> terminal_tht_hole_default, -- not relevant here, see spec
						face					=> smt_pad_face,
						position				=> terminal_position,
						pad_shape_smt			=> smt_pad_shape,
						stop_mask_status_smt	=> smt_stop_mask_status,
						stop_mask_shape_smt		=> make_stop_mask_smt,
						solder_paste_status		=> smt_solder_paste_status,
						stencil_shape			=> make_stencil
						));

				-- clean up for next terminal
				smt_stop_mask_shape		:= stopmask_expand_mode_default;
				delete_segments (smt_stop_mask_contours);
				delete_segments (smt_pad_shape);
				smt_stop_mask_status	:= stop_mask_status_default;
				smt_solder_paste_status	:= solder_paste_status_default;
				smt_stencil_shape		:= stencil_modification_default;
				delete_segments (smt_stencil_contours);
				smt_stencil_shrink		:= stencil_shrink_default;
		end case;

		if not inserted then
			log (ERROR, "terminal" & to_string (terminal_name) 
					& " already used !", console => true);
			raise constraint_error;
		end if;

		-- clean up for next terminal
		terminal_position	:= origin_zero_rotation;
		
	end build_terminal;

	
end et_package_read_terminal;
