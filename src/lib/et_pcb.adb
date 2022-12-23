------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                PCB                                       --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.strings.unbounded;
with ada.exceptions;
with ada.tags;

with et_text;					use et_text;

package body et_pcb is



	-- NET CLASSES
	function to_string (net_class_name : in pac_net_class_name.bounded_string) return string is
	begin
		return pac_net_class_name.to_string (net_class_name);
	end to_string;

	function to_net_class_name (net_class_name : in string) return pac_net_class_name.bounded_string is
	begin
		return pac_net_class_name.to_bounded_string (net_class_name);
	end to_net_class_name;
	
	function to_string (class_description : in pac_net_class_description.bounded_string) return string is
	begin
		return pac_net_class_description.to_string (class_description);
	end to_string;

	function to_net_class_description (class_description : in string) return pac_net_class_description.bounded_string is
	begin
		return pac_net_class_description.to_bounded_string (class_description);
	end to_net_class_description;

	
	function signal_layer_to_mirror (
		current_layer	: in et_pcb_stack.type_signal_layer;
		bottom_layer	: in et_pcb_stack.type_signal_layer)
		return et_text.type_vector_text_mirrored 
	is
		use et_text;
	begin
		if current_layer = bottom_layer then
			return YES;
		else
			return NO;
		end if;
	end signal_layer_to_mirror;



	
	function to_string (meaning : in type_text_meaning_conductor) return string is begin
		return to_lower (type_text_meaning_conductor'image (meaning));
	end to_string;

	function to_meaning (meaning : in string) return type_text_meaning_conductor is begin
		return type_text_meaning_conductor'value (meaning);
	end to_meaning;


		

	function to_string (locked : in type_locked) return string is begin
		return to_lower (type_locked'image (locked));
	end;

	function to_lock_status (locked : in string) return type_locked is begin
		return type_locked'value (locked);
	end;


	
	function package_position (position : in type_package_position) return string is
	begin
		return (" position" & to_string (position.place)
			& " angle" & to_string (get_rotation (position))
			& " face" & to_string (get_face (position)));
	end package_position;

	

	
	function to_mirror (
		flipped : in type_flipped)
		return et_text.type_vector_text_mirrored 
	is
		use et_text;
	begin
		case flipped is
			when YES => return YES;
			when NO => return NO;
		end case;
	end to_mirror;


	


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
	

	function get_conductor_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		use pac_devices_non_electric;
		device : type_device_non_electric renames element (device_cursor);
		result : pac_polygon_list.list;

		package_cursor : pac_packages_lib.cursor;
		
		package_displacement : constant type_distance_relative :=
			to_distance_relative (device.position.place);

		contours : pac_contour_list.list;

		use et_contour_to_polygon;
	begin
		package_cursor := packages_lib.find (device.package_model);

		if device.flipped = NO then
			contours := get_terminal_contours (package_cursor, layer_category);
			--put_line (to_string (contours.last_element));
			-- CS get_text_contours
			-- CS get_conductor_contours
			-- CS fill zones
			rotate_contours (contours, device.position.rotation);
		else
			contours := get_terminal_contours (package_cursor, invert_category (layer_category));
			mirror_contours (contours);
			rotate_contours (contours, - device.position.rotation);
		end if;

		move_contours (contours, package_displacement);

		
		return to_polygons (
			contours	=> contours,
			tolerance	=> fill_tolerance,
			mode		=> EXPAND,
			debug		=> false);

	end get_conductor_polygons;
	

	
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
