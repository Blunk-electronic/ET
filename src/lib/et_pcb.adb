------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;

with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.strings.maps;				use ada.strings.maps;

with ada.strings.unbounded;
with ada.exceptions;

with et_string_processing;				use et_string_processing;



package body et_pcb is


	function face_to_mirror (
		f : in type_face)
		return type_mirror
	is begin
		case f is
			when TOP	=> return MIRROR_NO;
			when BOTTOM	=> return MIRROR_ALONG_Y_AXIS;
		end case;		
	end face_to_mirror;

	


	
	function signal_layer_to_mirror (
		current_layer	: in et_pcb_stack.type_signal_layer;
		bottom_layer	: in et_pcb_stack.type_signal_layer)
		return type_mirror 
	is
		use et_text;
	begin
		if current_layer = bottom_layer then
			return MIRROR_ALONG_Y_AXIS;
		else
			return MIRROR_NO;
		end if;
	end signal_layer_to_mirror;



	
	

	

	function to_mirror_along_y_axis (
		flipped : in type_flipped)
		return type_mirror
	is begin
		case flipped is
			when YES => return MIRROR_ALONG_Y_AXIS;
			when NO =>  return MIRROR_NO;
		end case;
	end to_mirror_along_y_axis;

	


-- PROPERTIES OF ELECTRIC OBJECTS IN SIGNAL LAYERS
	
	procedure route_line_properties (
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_conductor_lines;
		line : type_conductor_line;
	begin
		line := element (cursor);
		log (text => "segment " & to_string (type_line (line)) &
			 " width" & to_string (line.width) &
			 " layer" & to_string (line.layer)
			 -- CS locked
			 , level => log_threshold);
	end route_line_properties;

	
	procedure route_via_properties (
		cursor			: in pac_vias.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_vias;
		
		procedure do_it (via : type_via) 
		is begin
			case via.category is
				when THROUGH =>
					log (text => "via" 
						& " category " & to_string (via.category) & space
						& to_string (type_drill (via)) 
						& " restring outer" & to_string (via.restring_outer) -- outer layers
						& " restring inner" & to_string (via.restring_inner), -- inner layers
						level => log_threshold);

				when others => null;
				-- CS log properties of other via categories
				
			end case;
						--& " layer_start" & to_string (via.layers.l_start) &
			 --" layer_end" & to_string (via.layers.l_end)
			 ---- CS locked
		end do_it;
	
	begin
		do_it (element (cursor));
	
	end route_via_properties;

		



	procedure pcb_contour_segment_properties (
		cursor			: in pac_segments.cursor;
		log_threshold 	: in type_log_level)
	is 
		use pac_segments;
	begin
		case element (cursor).shape is
			when LINE =>
				log (text => "PCB contour (edge cuts / outline) line" & space
					 & to_string (element (cursor).segment_line),
					 level => log_threshold);

			when ARC =>
				log (text => "PCB contour (edge cuts / outline) arc" & space
					 & to_string (element (cursor).segment_arc),
					 level => log_threshold);
				
		end case;
	end pcb_contour_segment_properties;

	
	procedure pcb_contour_circle_properties (
		circle			: in type_circle;
		log_threshold 	: in type_log_level)
	is begin
		log (text => "PCB contour (edge cuts / outline) circle" & space 
			 & to_string (circle),
			 level => log_threshold);
	end pcb_contour_circle_properties;
	



	procedure set_proposed (
		device : in out type_device_non_electric)
	is begin
		set_proposed (device.status);
	end;

	
	procedure clear_proposed (
		device : in out type_device_non_electric)
	is begin
		clear_proposed (device.status);
	end;

	
	function is_proposed (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_proposed (device.status) then
			return true;
		else
			return false;
		end if;
	end;
	

	

	procedure set_selected (
		device : in out type_device_non_electric)
	is begin
		set_selected (device.status);
	end;

	
	procedure clear_selected (
		device : in out type_device_non_electric)
	is begin
		clear_selected (device.status);
	end;
	
	
	function is_selected (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_selected (device.status) then
			return true;
		else
			return false;
		end if;
	end;




	procedure set_moving (
		device : in out type_device_non_electric)
	is begin
		set_moving (device.status);
	end;

	
	procedure clear_moving (
		device : in out type_device_non_electric)
	is begin
		clear_moving (device.status);
	end;

	
	function is_moving (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_moving (device.status) then
			return true;
		else
			return false;
		end if;
	end;
	



	procedure set_locked (
		device : in out type_device_non_electric)
	is begin
		set_locked (device.status);
	end;

	
	procedure clear_locked (
		device : in out type_device_non_electric)
	is begin
		clear_locked (device.status);
	end;

	
	function is_locked (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_locked (device.status) then
			return true;
		else
			return false;
		end if;
	end;


	


	function is_proposed (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_proposed (element (device)) then
			return true;
		else
			return false;
		end if;
	end;
	
	

	function is_selected (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_selected (element (device)) then
			return true;
		else
			return false;
		end if;
	end;


	
	function is_moving (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_moving (element (device)) then
			return true;
		else
			return false;
		end if;
	end;
	

	function is_locked (
		device : in pac_devices_non_electric.cursor)
		return boolean		
	is begin
		if is_locked (element (device)) then
			return true;
		else
			return false;
		end if;
	end;


		
	
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean)
	is
		use pac_devices_non_electric;
		c : pac_devices_non_electric.cursor := devices.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	
	
end et_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
