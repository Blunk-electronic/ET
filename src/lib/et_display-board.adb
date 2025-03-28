------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         LAYER DISPLAY BOARD                              --
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
--   ToDo: 

with ada.text_io;
with ada.strings;
with ada.strings.unbounded;

package body et_display.board is

	
	function board_contour_enabled return boolean is begin
		if layers.outline = ON then 
			return true;
		else
			return false;
		end if;
	end board_contour_enabled;

	

	procedure enable_board_contour is begin
		layers.outline := ON;
	end;


	
	procedure disable_board_contour is begin
		layers.outline := OFF;
	end;


	
	
	function plated_millings_enabled return boolean is begin
		if layers.plated_millings = ON then 
			return true;
		else
			return false;
		end if;
	end plated_millings_enabled;



	
	
	function silkscreen_enabled (
		face : in type_face) 
		return boolean 
	is begin
		case face is
			when TOP =>
				if layers.silkscreen.top = ON then return true; end if;
			when BOTTOM =>
				if layers.silkscreen.bottom = ON then return true; end if;
		end case;
		return false;
	end silkscreen_enabled;


	
	procedure enable_silkscreen (
		face : in type_face)
	is begin
		case face is
			when TOP => layers.silkscreen.top := ON;
			when BOTTOM => layers.silkscreen.bottom := ON;
		end case;
	end enable_silkscreen;





	
	
	function assy_doc_enabled (
		face : in type_face) 
		return boolean 
	is begin
		case face is
			when TOP =>
				if layers.assy_doc.top = ON then return true; end if;
			when BOTTOM =>
				if layers.assy_doc.bottom = ON then return true; end if;
		end case;
		return false;
	end assy_doc_enabled;


	
	procedure enable_assy_doc (
		face : in type_face)
	is begin
		case face is
			when TOP => layers.assy_doc.top := ON;
			when BOTTOM => layers.assy_doc.bottom := ON;
		end case;
	end enable_assy_doc;




	
	
	function keepout_enabled (
		face : in type_face) 
		return boolean 
	is begin
		case face is
			when TOP =>
				if layers.keepout.top = ON then return true; end if;
			when BOTTOM =>
				if layers.keepout.bottom = ON then return true; end if;
		end case;
		return false;
	end keepout_enabled;

	

	procedure enable_keepout (
		face : in type_face)
	is begin
		case face is
			when TOP => layers.keepout.top := ON;
			when BOTTOM => layers.keepout.bottom := ON;
		end case;
	end enable_keepout;




	



	
	function stop_mask_enabled (
		face : in type_face) 
		return boolean 
	is begin
		case face is
			when TOP =>
				if layers.stop_mask.top = ON then return true; end if;
			when BOTTOM =>
				if layers.stop_mask.bottom = ON then return true; end if;
		end case;
		return false;
	end stop_mask_enabled;

	
	
	procedure enable_stopmask (
		face : in type_face)
	is begin
		case face is
			when TOP => layers.stop_mask.top := ON;
			when BOTTOM => layers.stop_mask.bottom := ON;
		end case;
	end enable_stopmask;



	


	
	function stencil_enabled (
		face : in type_face) 
		return boolean 
	is begin
		case face is
			when TOP =>
				if layers.stencil.top = ON then return true; end if;
			when BOTTOM =>
				if layers.stencil.bottom = ON then return true; end if;
		end case;
		return false;
	end stencil_enabled;


	
	procedure enable_stencil (
		face : in type_face)
	is begin
		case face is
			when TOP => layers.stencil.top := ON;
			when BOTTOM => layers.stencil.bottom := ON;
		end case;
	end enable_stencil;





	
	
	function device_origins_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if layers.device_origins.top = ON then return true; end if;
			when BOTTOM =>
				if layers.device_origins.bottom = ON then return true; end if;
		end case;
		return false;
	end device_origins_enabled;


	function ratsnest_enabled return boolean is begin
		if layers.ratsnest = ON then
			return true;
		else 
			return false;
		end if;
	end ratsnest_enabled;
	
	
	function vias_enabled return boolean is begin
		if layers.vias = ON then
			return true;
		else
			return false;
		end if;
	end vias_enabled;





	

	function conductors_enabled return boolean is begin
		for r in type_conductors'first .. type_conductors'last loop
			if layers.conductors (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end conductors_enabled;

	
	function inner_conductors_enabled (
		deepest_layer : in type_signal_layer) -- the deepest conductor layer of the board
		return boolean is 
	begin
		for r in type_conductors'first + 1 .. deepest_layer - 1 loop
			if layers.conductors (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end inner_conductors_enabled;


	
	function conductor_enabled (
		layer : in type_signal_layer) 
	return boolean is begin
		if layers.conductors (layer) = ON then
			return true;
		else
			return false;
		end if;
	end conductor_enabled;


	procedure enable_conductor (
		layer : in type_signal_layer)
	is begin
		layers.conductors (layer) := ON;
	end enable_conductor;
	
	

	

	
	
	function route_restrict_enabled 
		return boolean 
	is begin
		for r in type_route_restrict'first .. type_route_restrict'last loop
			if layers.route_restrict (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end route_restrict_enabled;

	
	function route_restrict_layer_enabled (
		layer : in type_signal_layer) 
		return boolean 
	is begin
		if layers.route_restrict (layer) = ON then
			return true;
		else
			return false;
		end if;
	end route_restrict_layer_enabled;

	
	
	function route_restrict_layer_enabled (
		layers : in type_signal_layers.set)
		return boolean 
	is
		result : boolean := false;
	begin
		for r in type_route_restrict'first .. type_route_restrict'last loop

			if et_display.board.layers.route_restrict (r) = ON then
				if layers.contains (r) then
					result := true;
					exit; -- no need to probe remaining layers
				end if;
			end if;
			
		end loop;

		return result;
	end route_restrict_layer_enabled;


	
	function route_restrict_enabled (
		face 			: in type_face;
		deepest_layer	: in type_signal_layer)
		return boolean
	is begin
		case face is
			when TOP =>
				if layers.route_restrict (1) = ON then return true; end if;
			when BOTTOM =>
				if layers.route_restrict (deepest_layer) = ON then return true; end if;
		end case;
		return false;
	end route_restrict_enabled;


	
	procedure enable_route_restrict (
		layer : in type_signal_layer)
	is begin
		layers.route_restrict (layer) := ON;
	end enable_route_restrict;





	
	

	function via_restrict_enabled return boolean is begin
		for r in type_via_restrict'first .. type_via_restrict'last loop
			if layers.via_restrict (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end via_restrict_enabled;

	
	function via_restrict_layer_enabled (
		layer : in type_signal_layer) 
		return boolean 
	is begin
		if layers.via_restrict (layer) = ON then
			return true;
		else
			return false;
		end if;
	end via_restrict_layer_enabled;


	
	procedure enable_via_restrict (
		layer : in type_signal_layer)
	is begin
		layers.via_restrict (layer) := ON;
	end enable_via_restrict;


	
	function via_restrict_layer_enabled (layers : in type_signal_layers.set)
		return boolean is
		result : boolean := false;
	begin
		for r in type_via_restrict'first .. type_via_restrict'last loop

			if et_display.board.layers.via_restrict (r) = ON then
				if layers.contains (r) then
					result := true;
					exit; -- no need to probe remaining layers
				end if;
			end if;
			
		end loop;

		return result;
	end via_restrict_layer_enabled;


	function via_restrict_enabled (
		face 			: in type_face;
		deepest_layer	: in type_signal_layer)
		return boolean
	is begin
		case face is
			when TOP =>
				if layers.via_restrict (1) = ON then return true; end if;
			when BOTTOM =>
				if layers.via_restrict (deepest_layer) = ON then return true; end if;
		end case;
		return false;
	end via_restrict_enabled;

	

	
	function enabled_conductor_layers return string is
		use ada.text_io;
		use ada.strings;
		use ada.strings.unbounded;

		ly : unbounded_string;
		separator_1 : constant character := ',';
		separator_2 : constant string := "..";

		in_range : boolean := false;
		min, max : type_signal_layer := type_signal_layer'first;
	begin
		for l in type_signal_layer'first .. type_signal_layer'last loop

			if layers.conductors (l) = ON then
				if not in_range then
					min := l;
					in_range := true;
				end if;
								
			else -- OFF
				if in_range then
					max := l - 1;
					in_range := false;

					if min = max then -- avoids output like "2..2"
						ly := ly & to_unbounded_string (to_string (min) & separator_1);
					else
						ly := ly & to_unbounded_string (to_string (min) &
								separator_2 & to_string (max) & separator_1);
					end if;
					
				end if;
			end if;
			
		end loop;

		-- Remove the trailing separator_1:
		return slice (ly, 1, length (ly) - 1);
	end enabled_conductor_layers;

	
end et_display.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
