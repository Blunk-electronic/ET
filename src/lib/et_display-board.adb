------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         LAYER DISPLAY BOARD                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
with ada.strings.bounded;

package body et_display.board is

	function outline_enabled return boolean is begin
		if board_layers.outline = ON then 
			return true;
		else
			return false;
		end if;
	end outline_enabled;

	function plated_millings_enabled return boolean is begin
		if board_layers.plated_millings = ON then 
			return true;
		else
			return false;
		end if;
	end plated_millings_enabled;
	
	function silkscreen_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if board_layers.silkscreen.top = ON then return true; end if;
			when BOTTOM =>
				if board_layers.silkscreen.bottom = ON then return true; end if;
		end case;
		return false;
	end silkscreen_enabled;

	function assy_doc_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if board_layers.assy_doc.top = ON then return true; end if;
			when BOTTOM =>
				if board_layers.assy_doc.bottom = ON then return true; end if;
		end case;
		return false;
	end assy_doc_enabled;

	function keepout_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if board_layers.keepout.top = ON then return true; end if;
			when BOTTOM =>
				if board_layers.keepout.bottom = ON then return true; end if;
		end case;
		return false;
	end keepout_enabled;

	function stop_mask_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if board_layers.stop_mask.top = ON then return true; end if;
			when BOTTOM =>
				if board_layers.stop_mask.bottom = ON then return true; end if;
		end case;
		return false;
	end stop_mask_enabled;

	function stencil_enabled (face : in type_face) return boolean is begin
		case face is
			when TOP =>
				if board_layers.stencil.top = ON then return true; end if;
			when BOTTOM =>
				if board_layers.stencil.bottom = ON then return true; end if;
		end case;
		return false;
	end stencil_enabled;
	
	function route_restrict_enabled return boolean is begin
		for r in type_route_restrict'first .. type_route_restrict'last loop
			if board_layers.route_restrict (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end route_restrict_enabled;

	function via_restrict_enabled return boolean is begin
		for r in type_via_restrict'first .. type_via_restrict'last loop
			if board_layers.via_restrict (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end via_restrict_enabled;

	function conductors_enabled return boolean is begin
		for r in type_conductors'first .. type_conductors'last loop
			if board_layers.conductors (r) = ON then
				return true;
			end if;
		end loop;

		return false;
	end conductors_enabled;

	function conductor_enabled (layer : in type_signal_layer) return boolean is begin
		if board_layers.conductors (layer) = ON then
			return true;
		else
			return false;
		end if;
	end conductor_enabled;
	
	function route_restrict_layer_enabled (layers : in type_signal_layers.set)
		return boolean is
		result : boolean := false;
	begin
		for r in type_route_restrict'first .. type_route_restrict'last loop

			if board_layers.route_restrict (r) = ON then
				if layers.contains (r) then
					result := true;
					exit; -- no need to probe remaining layers
				end if;
			end if;
			
		end loop;

		return result;
	end route_restrict_layer_enabled;

	function via_restrict_layer_enabled (layers : in type_signal_layers.set)
		return boolean is
		result : boolean := false;
	begin
		for r in type_via_restrict'first .. type_via_restrict'last loop

			if board_layers.via_restrict (r) = ON then
				if layers.contains (r) then
					result := true;
					exit; -- no need to probe remaining layers
				end if;
			end if;
			
		end loop;

		return result;
	end via_restrict_layer_enabled;

	function enabled_conductor_layers return string is
		use ada.text_io;
		use ada.strings;
 		use ada.strings.bounded;
		package pac_layers is new generic_bounded_length (positive (2 * type_signal_layer'last)); -- CS sufficent long ?
		use pac_layers;
		layers : pac_layers.bounded_string;
		separator_1 : constant character := space;
		separator_2 : constant string := "..";
	begin
		for l in type_signal_layer'first .. type_signal_layer'last loop

			if board_layers.conductors (l) = ON then
				layers := layers & to_bounded_string (separator_1 & to_string (l));
			end if;
			
		end loop;
		
		return to_string (layers);
	end enabled_conductor_layers;

	
end et_display.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
