------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES NON-ELECTRICAL                           --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;					use ada.text_io;
with ada.characters.latin_1;
with ada.strings.unbounded;

with et_contour_to_polygon;
with et_stopmask.packages;
with et_silkscreen.packages;
with et_assy_doc.packages;
with et_logging;
with et_string_processing;			use et_string_processing;


package body et_devices_non_electrical is


	procedure device_name_in_use (
		name : in type_device_name)
	is 
		use et_logging;
	begin
		log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
			 " already used by another non-electrical device !",
			 console => true);
		
		raise constraint_error;
	end device_name_in_use;



	
	function to_mirror_along_y_axis (
		flipped : in type_flipped)
		return type_mirror
	is begin
		case flipped is
			when YES => return MIRROR_ALONG_Y_AXIS;
			when NO =>  return MIRROR_NO;
		end case;
	end to_mirror_along_y_axis;

	


	function get_package_model_name (
		device	: in type_device_non_electric)
		return pac_package_model_file_name.bounded_string
	is begin
		return device.package_model;
	end;

	


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


	



	procedure modify_status (
		device		: in out type_device_non_electric;
		operation	: in type_status_operation)						
	is begin
		modify_status (device.status, operation);
	end;

	


	procedure reset_status (
		device : in out type_device_non_electric)
	is begin
		reset_status (device.status);
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


	
	function get_device_name (
		device : in pac_devices_non_electric.cursor)
		return type_device_name
	is begin
		return key (device);
	end get_device_name;


	
	
	function get_device_name (
		device : in pac_devices_non_electric.cursor)
		return string
	is begin
		return to_string (key (device));
	end get_device_name;

	
	

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
		



	function get_package_model (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_package_models.cursor
	is 
		result : pac_package_models.cursor;
	begin
		result := get_package_model (element (device_cursor).package_model);
		return result;
	end get_package_model;





	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_package_position
	is begin
		return element (device_cursor).position;
	end get_position;


	

	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_vector_model
	is begin
		return get_position (device_cursor).place;
	end get_position;



	
	function get_face (
		device_cursor	: in pac_devices_non_electric.cursor)
		return type_face
	is 
		position : type_package_position;
	begin
		position := element (device_cursor).position;
		return get_face (position);
	end get_face;
	






	function get_device_properties (
		device		: in type_device_non_electric;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string
	is
		use ada.strings.unbounded;
		result : unbounded_string;


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is 
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end;
		

		
		procedure get_info_1 is begin
			-- CS
			null;
			-- result := to_unbounded_string (" value: " & to_string (device.value) & ins_LF);
		end;


		procedure get_info_2 is begin
			null;
			-- CS
			-- if is_real (device) then
			-- 	result := result & " partcode: " 
				-- & to_string (device.partcode) & ins_LF;
			-- end if;
		end;


		procedure get_info_3 is begin
			result := result & " package model: "
				& to_string (get_package_model_name (device)) & ins_LF;
		end;


		
	begin
		case level is
			when DEVICE_PROPERTIES_LEVEL_1 =>
				get_info_1;
  
			when DEVICE_PROPERTIES_LEVEL_2 =>
				get_info_1; 
				get_info_2; 
  
			when DEVICE_PROPERTIES_LEVEL_3 =>
				get_info_1; 
				get_info_2; 
				get_info_3;
		end case;

		return to_string (result);
	end get_device_properties;






	

	function get_properties (
		device_cursor	: in pac_devices_non_electric.cursor;
		level			: in type_properties_level;
		linebreaks		: in boolean := false)
		return string
	is
		use ada.strings.unbounded;
		units_info : unbounded_string;

		device : type_device_non_electric renames element (device_cursor);


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is 
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end;

		
	begin
		-- CS

		-- Get general properties of the device.
		-- Append the properties of the requested unit:
		return "device: " & get_device_name (device_cursor) & ins_LF
			& get_device_properties (device, level, linebreaks) & ins_LF;

	end get_properties;

	


	

	function get_conductor_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is
		conductors : type_conductor_objects; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		offset : constant type_vector_model := device.position.place;
	begin
		-- lines, arcs, circles, texts
		if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
			case get_face (device_cursor) is
				when TOP =>
					conductors := get_conductor_objects (packge, layer_category);
					rotate_conductor_objects (conductors, + device.position.rotation);

				when BOTTOM =>
					conductors := get_conductor_objects (packge, invert_category (layer_category));
					mirror_conductor_objects (conductors);
					rotate_conductor_objects (conductors, - device.position.rotation);
			end case;

			move_conductor_objects (conductors, offset);
		end if;

		return conductors;
	end get_conductor_objects;






	function get_conductor_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		offset : constant type_vector_model := device.position.place;

		use pac_contours;
		terminals	: pac_contour_list.list;
		
		conductors	: type_conductor_objects; -- non-electrical
		conductor_polygons : pac_polygon_list.list;

		use et_contour_to_polygon;
	begin
		-- TERMINALS:
		case get_face (device_cursor) is
			when TOP =>
				terminals := get_terminal_contours (packge, layer_category);
				rotate_contours (terminals, device.position.rotation);
			when BOTTOM =>
				terminals := get_terminal_contours (packge, invert_category (layer_category));
				mirror_contours (terminals);
				rotate_contours (terminals, - device.position.rotation);
		end case;

		move_contours (terminals, offset);
		
		result := to_polygons (
			contours	=> terminals,
			tolerance	=> fill_tolerance,
			mode		=> EXPAND,
			debug		=> false);


		-- CONDUCTOR OBJECTS (lines, arcs, circles, texts)
		if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
			case get_face (device_cursor) is
				when TOP =>
					conductors := get_conductor_objects (packge, layer_category);
					rotate_conductor_objects (conductors, + device.position.rotation);
					
				when BOTTOM =>
					conductors := get_conductor_objects (packge, invert_category (layer_category));
					mirror_conductor_objects (conductors);
					rotate_conductor_objects (conductors, - device.position.rotation);
			end case;

			move_conductor_objects (conductors, offset);

			-- convert conductor objects to polygons:
			conductor_polygons := to_polygons (conductors, fill_tolerance);
		end if;
		
		result.splice (before => pac_polygon_list.no_element, source => conductor_polygons);

		return result;
	end get_conductor_polygons;
	



	

	function get_route_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side
	is
		use et_route_restrict.packages;
		restrict : type_one_side; -- to be returned
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		if layer_category /= INNER then -- route restrict objects exist in outer layers only
			case get_face (device_cursor) is
				when TOP =>
					restrict := get_route_restrict_objects (packge, layer_category);
					rotate_route_restrict_objects (restrict, + device.position.rotation);

				when BOTTOM =>
					restrict := get_route_restrict_objects (packge, invert_category (layer_category));
					mirror_route_restrict_objects (restrict);
					rotate_route_restrict_objects (restrict, - device.position.rotation);
			end case;

			move_route_restrict_objects (restrict, device.position.place);
		end if;

		return restrict;
	end get_route_restrict_objects;


	
	
	
	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		use et_route_restrict.packages;
		restrict : type_one_side;
	begin
		if layer_category /= INNER then -- route restrict objects exist in outer layers only
			case get_face (device_cursor) is
				when TOP =>
					restrict := get_route_restrict_objects (packge, layer_category);
					rotate_route_restrict_objects (restrict, + device.position.rotation);

				when BOTTOM =>
					restrict := get_route_restrict_objects (packge, invert_category (layer_category));
					mirror_route_restrict_objects (restrict);
					rotate_route_restrict_objects (restrict, - device.position.rotation);
			end case;

			move_route_restrict_objects (restrict, device.position.place);

			-- convert restrict objects to polygons:
			result := to_polygons (restrict, fill_tolerance);
		end if;

		return result;
	end get_route_restrict_polygons;
	



	function get_via_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side
	is
		use et_via_restrict.packages;		
		restrict : type_one_side; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		if layer_category /= INNER then -- via restrict objects exist in outer layers only
			case get_face (device_cursor) is
				when TOP =>
					restrict := get_via_restrict_objects (packge, layer_category);
					rotate_via_restrict_objects (restrict, + device.position.rotation);

				when BOTTOM =>
					restrict := get_via_restrict_objects (packge, invert_category (layer_category));
					mirror_via_restrict_objects (restrict);
					rotate_via_restrict_objects (restrict, - device.position.rotation);
			end case;

			move_via_restrict_objects (restrict, device.position.place);
		end if;

		return restrict;
	end get_via_restrict_objects;
	




	function get_keepout_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_keepout
	is
		result : type_keepout;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		case face is
			when TOP =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_keepout_objects (packge, TOP);
						rotate_keepout_objects (result, + rotation);

					when BOTTOM =>
						result := get_keepout_objects (packge, BOTTOM);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
				end case;

			when BOTTOM =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_keepout_objects (packge, BOTTOM);
						rotate_keepout_objects (result, + rotation);

					when BOTTOM =>
						result := get_keepout_objects (packge, TOP);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
				end case;
		end case;

		move_keepout_objects (result, device.position.place);
		
		return result;
	end get_keepout_objects;




	

	function get_stencil_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stencil
	is
		result : type_stencil;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		case face is
			when TOP =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_stencil_objects (packge, TOP);
						rotate_stencil_objects (result, + rotation);

					when BOTTOM =>
						result := get_stencil_objects (packge, BOTTOM);
						mirror_stencil_objects (result);
						rotate_stencil_objects (result, - rotation);
				end case;

			when BOTTOM =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_stencil_objects (packge, BOTTOM);
						rotate_stencil_objects (result, + rotation);
						
					when BOTTOM =>
						result := get_stencil_objects (packge, TOP);
						mirror_stencil_objects (result);
						rotate_stencil_objects (result, - rotation);
				end case;
		end case;
		
		move_stencil_objects (result, device.position.place);
		return result;
	end get_stencil_objects;

	



	
	function get_stopmask_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stopmask
	is
		result : type_stopmask;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_stopmask.packages;
	begin
		case face is
			when TOP =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_stopmask_objects (packge, TOP);
						rotate_stopmask_objects (result, + rotation);

					when BOTTOM =>
						result := get_stopmask_objects (packge, BOTTOM);
						mirror_stopmask_objects (result);
						rotate_stopmask_objects (result, - rotation);
				end case;

			when BOTTOM =>
				case get_face (device_cursor) is
					when TOP =>
						result := get_stopmask_objects (packge, BOTTOM);
						rotate_stopmask_objects (result, + rotation);

					when BOTTOM =>
						result := get_stopmask_objects (packge, TOP);
						mirror_stopmask_objects (result);
						rotate_stopmask_objects (result, - rotation);
				end case;
		end case;
		
		move_stopmask_objects (result, device.position.place);
		return result;
	end get_stopmask_objects;




	
	function to_placeholder_content (
		device_cursor	: in pac_devices_non_electric.cursor;
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string 
	is
		device : type_device_non_electric renames element (device_cursor);

		use et_text;
		result : pac_text_content.bounded_string;
	begin
		case placeholder.meaning is
			when NAME 		=> result := to_content (to_string (key (device_cursor)));
			-- CS
			--when VALUE		=> result := to_content (to_string (device.value));
			--when PURPOSE	=> result := to_content (to_string (device.purpose));
			when others => null;
		end case;
		
		return result;
	end to_placeholder_content;
	





	function get_silkscreen_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_silkscreen
	is
		result : type_silkscreen;		

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_silkscreen.packages;
		silkscreen : et_silkscreen.packages.type_silkscreen_package;

		
		-- Converts the placeholders to a list of regular texts
		-- and appends them to the silkscreen.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_silk_text := (type_text_fab (ph) with others => <>);
				use et_text;
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					silkscreen.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			silkscreen.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;
	
		
	begin -- get_silkscreen_objects
		case face is
			when TOP =>
				case get_face (device_cursor) is
					when TOP =>
						silkscreen := get_silkscreen_objects (packge, TOP);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.top;
						convert_placeholders_to_texts;
						rotate_silkscreen_objects (silkscreen, + rotation);
						
					when BOTTOM =>
						silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
						convert_placeholders_to_texts;
						mirror_silkscreen_objects (silkscreen);
						rotate_silkscreen_objects (silkscreen, - rotation);
				end case;

				
			when BOTTOM =>
				case get_face (device_cursor) is
					when TOP =>
						silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
						convert_placeholders_to_texts;
						rotate_silkscreen_objects (silkscreen, + rotation);

					when BOTTOM =>
						silkscreen := get_silkscreen_objects (packge, TOP);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.top;
						convert_placeholders_to_texts;
						mirror_silkscreen_objects (silkscreen);
						rotate_silkscreen_objects (silkscreen, - rotation);
				end case;
		end case;
		
		move_silkscreen_objects (silkscreen, device.position.place);

		result := type_silkscreen (silkscreen);
		return result;
	end get_silkscreen_objects;






	
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_assy_doc
	is
		result : type_assy_doc;		

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_assy_doc.packages;
		assy_doc : et_assy_doc.packages.type_assy_doc_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the assy_doc.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_doc_text := (type_text_fab (ph) with others => <>);
				use et_text;
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					
					-- Vectorize the content of the placeholder:
					-- text.vectors := vectorize_text (
					-- 	content		=> text.content,
					-- 	size		=> ph.size,
					-- 	--rotation	=> add (get_rotation (ph.position), get_rotation (package_position)),
					-- 	rotation	=> get_rotation (ph.position),
					-- 	position	=> ph.position.place,
					-- 	mirror		=> MIRROR_NO,
					-- 	line_width	=> ph.line_width,
					-- 	alignment	=> ph.alignment); -- right, bottom

					assy_doc.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			assy_doc.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;

		
	begin -- get_assy_doc_objects
		case face is
			when TOP =>
				case get_face (device_cursor) is
					when TOP =>
						assy_doc := get_assy_doc_objects (packge, TOP);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.top;
						convert_placeholders_to_texts;
						rotate_assy_doc_objects (assy_doc, + rotation);

					when BOTTOM =>
						assy_doc := get_assy_doc_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
						convert_placeholders_to_texts;
						mirror_assy_doc_objects (assy_doc);
						rotate_assy_doc_objects (assy_doc, - rotation);
				end case;

			when BOTTOM =>
				case get_face (device_cursor) is
					when TOP =>
						assy_doc := get_assy_doc_objects (packge, BOTTOM);

						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
						convert_placeholders_to_texts;
						rotate_assy_doc_objects (assy_doc, + rotation);

					when BOTTOM =>
						assy_doc := get_assy_doc_objects (packge, TOP);

						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.top;
						convert_placeholders_to_texts;
						mirror_assy_doc_objects (assy_doc);
						rotate_assy_doc_objects (assy_doc, - rotation);
				end case;
		end case;
		
		move_assy_doc_objects (assy_doc, device.position.place);

		result := type_assy_doc (assy_doc);
		return result;
	end get_assy_doc_objects;






	function get_holes (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_holes.list
	is
		holes : pac_holes.list; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
				
		case get_face (device_cursor) is
			when TOP =>
				mirror_holes (holes);
				rotate_holes (holes, - rotation);
				
			when BOTTOM =>
				rotate_holes (holes, + rotation);
		end case;
		
		move_holes (holes, device.position.place);
		return holes;
	end get_holes;


	
	
	function get_hole_polygons (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		holes : pac_holes.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		rotation : type_rotation_model renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
		
		case get_face (device_cursor) is
			when TOP =>
				mirror_holes (holes);
				rotate_holes (holes, - rotation);

			when BOTTOM =>
				rotate_holes (holes, + rotation);
				
		end case;
		
		move_holes (holes, device.position.place);
		
		result := to_polygons (holes, fill_tolerance);
		return result;
	end get_hole_polygons;

	
	
end et_devices_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
