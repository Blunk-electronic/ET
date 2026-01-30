------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICES ELECTRICAL / PACKAGES                       --
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
-- <http://www.gnu.org/licenses/>.   
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
with ada.characters.handling;
with ada.exceptions;

with et_contour_to_polygon;

with et_device_library.packages;	use et_device_library.packages;



package body et_devices_electrical.packages is


	
-- VALUE:

	procedure set_value (
		device	: in out type_device_electrical;
		value	: in pac_device_value.bounded_string)
	is begin
		device.value := value;
	end;


	function get_value (
		device	: in type_device_electrical)
		return pac_device_value.bounded_string
	is begin
		return device.value;
	end;


	function get_value (
		device	: in type_device_electrical)
		return string
	is begin
		return to_string (get_value (device));
	end;


	function get_value (
		device : in pac_devices_electrical.cursor)
		return pac_device_value.bounded_string 
	is begin
		return pac_devices_electrical.element (device).value;
	end get_value;



	function has_value (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_value (device)) then
			return false;
		else
			return true;
		end if;
	end;

	
	
	
-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_electrical;
		partcode	: in pac_device_partcode.bounded_string)
	is begin
		device.partcode := partcode;
	end;


	function get_partcode (
		device	: in type_device_electrical)
		return pac_device_partcode.bounded_string
	is begin
		return device.partcode;
	end;


	function get_partcode (
		device	: in type_device_electrical)
		return string
	is begin
		return to_string (get_partcode (device));
	end;


	
	function get_partcode (
		device : in pac_devices_electrical.cursor)
		return pac_device_partcode.bounded_string
	is begin
		return pac_devices_electrical.element (device).partcode;
	end get_partcode;



	function has_partcode (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_partcode (device)) then
			return false;
		else
			return true;
		end if;
	end;
	

	

-- PURPOSE:

	procedure set_purpose (
		device	: in out type_device_electrical;
		purpose	: in pac_device_purpose.bounded_string)
	is begin
		device.purpose := purpose;
	end;


	function get_purpose (
		device	: in type_device_electrical)
		return pac_device_purpose.bounded_string
	is begin
		return device.purpose;
	end;


	function get_purpose (
		device	: in type_device_electrical)
		return string
	is begin
		return to_string (get_purpose (device));
	end;



	function get_purpose (
		device : in pac_devices_electrical.cursor)
		return pac_device_purpose.bounded_string
	is begin
		return pac_devices_electrical.element (device).purpose;
	end get_purpose;

	

	function has_purpose (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_purpose (device)) then
			return false;
		else
			return true;
		end if;
	end;


	

-- PACKAGE VARIANTS:
	
	function get_package_variant (
		device : in type_device_electrical)
		return pac_package_variant_name.bounded_string
	is begin
		return device.variant;
	end;

	

	function get_available_package_variants (
		device : in type_device_electrical)
		return pac_package_variants.map
	is
		result : pac_package_variants.map;

		device_cursor_lib : pac_device_models.cursor;
	begin
		device_cursor_lib := get_device_model (device);

		result := get_available_variants (device_cursor_lib);

		return result;
	end get_available_package_variants;





	function get_package_variant (
		device : in pac_devices_electrical.cursor)
		return pac_package_variant_name.bounded_string
	is 
		d : type_device_electrical renames element (device);
	begin
		return get_package_variant (d);
	end get_package_variant;



	function get_available_package_variants (
		device : in pac_devices_electrical.cursor)
		return pac_package_variants.map
	is
		d : type_device_electrical renames element (device);
	begin
		return get_available_package_variants (d);
	end;


	


-- POSITION:

	procedure set_position (
		device		: in out type_device_electrical;
		position	: in type_package_position)
	is begin
		device.position := position;
	end;


	function get_position (
		device : in type_device_electrical)
		return type_package_position
	is begin
		return device.position;
	end;



	function get_position (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string
	is begin
		return to_string (device.position, format);
	end;

	

	function get_rotation (
		device	: in out type_device_electrical)
		return type_rotation_model
	is begin
		return get_rotation (device.position);
	end;

	
	procedure set_rotation (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
	is begin
		set_rotation (device.position, rotation);
	end;
	

	procedure set_rotation_relative (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
	is begin
		set_rotation_relative (device.position, rotation);
	end;


	
	procedure set_face (
		device	: in out type_device_electrical;
		face	: in type_face)
	is begin
		set_face (device.position, face);
	end;
	


	procedure toggle_face (
		device	: in out type_device_electrical)
	is begin
		toggle_face (device.position);
	end;
							  


	

	function get_face (
		device	: in type_device_electrical)
		return type_face
	is begin
		return get_face (device.position);
	end;



	function get_face (
		device	: in type_device_electrical)
		return string
	is begin
		return to_string (get_face (device.position));
	end;

	

	procedure set_place (
		device	: in out type_device_electrical;
		place	: in type_vector_model)
	is begin
		set_place (device.position, place);
	end;


	
	procedure set_place_relative (
		device	: in out type_device_electrical;
		offset	: in type_vector_model)
	is begin
		set_place_relative (device.position, offset);
	end;


	
	function get_place (
		device	: in type_device_electrical)
		return type_vector_model
	is begin
		return get_place (device.position);
	end;

	

	function get_place (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)		   
		return string
	is begin
		return et_board_geometry.pac_geometry_2.to_string (
			get_place (device.position), format);
	end;







	procedure log_package_position (
		device_cursor	: in pac_devices_electrical.cursor;
		log_threshold	: in type_log_level) 
	is
		use et_pcb_sides;		
		use pac_devices_electrical;
	begin
		if is_real (device_cursor) then
			log (text => "location in board:" & 
				to_string (element (device_cursor).position.place) &
				" face" & 
				to_string (get_face (element (device_cursor).position)),
				level => log_threshold);
		end if;
	end;


	



	
	function get_position (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return et_board_coordinates.type_package_position
	is 
		device : type_device_electrical renames element (device_cursor);
	begin
		return get_position (device);
	end get_position;


	
	function get_place (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return et_board_geometry.pac_geometry_2.type_vector_model
	is 
		device : type_device_electrical renames element (device_cursor);
	begin
		return get_place (device);
	end get_place;

	

	
	
	function get_face (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return type_face
	is 
		device : type_device_electrical renames element (device_cursor);
	begin
		return get_face (device);
	end get_face;





	

-- PLACEHOLDERS:

	
	procedure reset_placeholder_positions (
		device		: in out type_device_electrical)
	is 
		-- cursor_lib : pac_device_models.cursor;
		default_placeholders : type_text_placeholders;
	begin
		-- Locate the device in the device library:
		-- cursor_lib := get_device_model (device.model_name);

		-- Get the default placeholders as they are specified
		-- in the package model:
		default_placeholders := get_default_placeholders (
			device.model_cursor, get_package_variant (device));

		-- Assign the default placeholders to the given device:
		device.placeholders := default_placeholders;		
	end reset_placeholder_positions;
	


	
	
	
	procedure move_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;					 
		layer		: in type_placeholder_layer;
		face		: in type_face;
		index		: in type_placeholder_index;
		coordinates	: in type_coordinates;
		point		: in type_vector_model)
	is begin
		move_placeholder (device.placeholders, meaning, layer,
			face, index, get_position (device), coordinates, point);

	end move_placeholder;


	

	
	
	procedure rotate_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer;
		face		: in type_face;
		index		: in type_placeholder_index;
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model)
	is begin
		rotate_placeholder (device.placeholders, meaning, layer,
			face, index, coordinates, rotation);
		
	end rotate_placeholder;
	


	



	

-- CONDUCTOR OBJECTS:
	

	function get_conductor_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is
		conductors : type_conductor_objects; -- to be returned
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;

	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);

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

				move_conductor_objects (conductors, device.position.place);
			end if;
		end if;
		
		return conductors;
	end get_conductor_objects;
	

	




	function get_conductor_polygons (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		use et_board_geometry.pac_polygons;
		result : pac_polygon_list.list;
		use et_contour_to_polygon;
		
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		conductors : type_conductor_objects; -- non-electrical

	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);

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

				move_conductor_objects (conductors, device.position.place);

				-- convert conductor objects to polygons:
				result := to_polygons (conductors, fill_tolerance);
			end if;
		end if;
		
		return result;
	end get_conductor_polygons;


	
	
	


	
-- ROUTE RESTRICT
	
	function get_route_restrict_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side
	is	
		use et_route_restrict.packages;
		restrict : type_one_side; -- to be returned
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;

	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
				
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
		end if;

		return restrict;
	end get_route_restrict_objects;


	
	

	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;

		use et_route_restrict.packages;
		restrict : type_one_side;

		use et_board_geometry.pac_polygons;

		result : pac_polygon_list.list;
	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
				
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
		end if;
		
		return result;
	end get_route_restrict_polygons;
	





	
-- VIA RESTRICT
	
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side
	is		
		use et_via_restrict.packages;
		restrict : type_one_side; -- to be returned
		
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;

	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
				
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
		end if;

		return restrict;
	end get_via_restrict_objects;


	



	
-- KEEPOUT
	
	function get_keepout_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_keepout
	is
		result : type_keepout;
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
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
		end if;
		
		move_keepout_objects (result, device.position.place);
		return result;
	end get_keepout_objects;




	


	
	
-- STENCIL
	
	function get_stencil_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_stencil
	is
		result : type_stencil;
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;
	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
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
		end if;
		
		move_stencil_objects (result, device.position.place);
		return result;
	end get_stencil_objects;

	

	



	

-- STOPMASK:

	function get_stopmask_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_stopmask
	is
		result : type_stopmask;
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_stopmask.packages;
	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

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
		end if;

		return result;
	end get_stopmask_objects;





-- SILKSCREEN

	function get_silkscreen_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_silkscreen
	is
		use et_board_text;
		
		result : type_silkscreen;
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_silkscreen.packages;
		silkscreen : et_silkscreen.packages.type_silkscreen_package;

		
		-- Converts the placeholders to a list of regular texts
		-- and appends them to the silkscreen.texts:
		procedure convert_placeholders_to_texts is
			use pac_text_placeholders;

			
			procedure query_placeholder (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder renames element (c);
				use pac_text_board_vectorized;
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
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					case get_face (device_cursor) is
						when TOP =>
							silkscreen := get_silkscreen_objects (packge, TOP);

							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.placeholders.silkscreen.top;
							convert_placeholders_to_texts;
							rotate_silkscreen_objects (silkscreen, + rotation);
							
						when BOTTOM =>
							silkscreen := get_silkscreen_objects (packge, BOTTOM);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.placeholders.silkscreen.bottom;
							convert_placeholders_to_texts;
							mirror_silkscreen_objects (silkscreen);
							rotate_silkscreen_objects (silkscreen, - rotation);
					end case;

					
				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
						   silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.placeholders.silkscreen.bottom;
							convert_placeholders_to_texts;
							rotate_silkscreen_objects (silkscreen, + rotation);
							
						when BOTTOM =>
							silkscreen := get_silkscreen_objects (packge, TOP);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.placeholders.silkscreen.top;
							convert_placeholders_to_texts;
							mirror_silkscreen_objects (silkscreen);
							rotate_silkscreen_objects (silkscreen, - rotation);
					end case;
			end case;

			move_silkscreen_objects (silkscreen, device.position.place);
		end if;

		result := type_silkscreen (silkscreen);		
		return result;
	end get_silkscreen_objects;
	

	



-- ASSEMBLY DOCUMENTATION:
	
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_electrical.cursor;
		face			: in type_face)
		return type_assy_doc
	is
		use et_board_text;

		result : type_assy_doc;
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_assy_doc.packages;
		assy_doc : et_assy_doc.packages.type_assy_doc_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the assy_doc.texts:
		procedure convert_placeholders_to_texts is
			use pac_text_placeholders;

			
			procedure query_placeholder (c : in pac_text_placeholders.cursor) is
				ph : type_text_placeholder renames element (c);
				use pac_text_board_vectorized;
				text : type_doc_text := (type_text_fab (ph) with others => <>);
				use et_text;
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					assy_doc.texts.append (text);
				end if;
			end query_placeholder;

			
		begin
			assy_doc.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;

		
	begin -- get_assy_doc_objects
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					case get_face (device_cursor) is
						when TOP =>
							assy_doc := get_assy_doc_objects (packge, TOP);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.placeholders.assy_doc.top;
							convert_placeholders_to_texts;
							rotate_assy_doc_objects (assy_doc, + rotation);

						when BOTTOM =>
							assy_doc := get_assy_doc_objects (packge, BOTTOM);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.placeholders.assy_doc.bottom;
							convert_placeholders_to_texts;
							mirror_assy_doc_objects (assy_doc);
							rotate_assy_doc_objects (assy_doc, - rotation);
					end case;

				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
							assy_doc := get_assy_doc_objects (packge, BOTTOM);

							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.placeholders.assy_doc.bottom;
							convert_placeholders_to_texts;
							rotate_assy_doc_objects (assy_doc, + rotation);

						when BOTTOM =>
							assy_doc := get_assy_doc_objects (packge, TOP);

							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.placeholders.assy_doc.top;
							convert_placeholders_to_texts;
							mirror_assy_doc_objects (assy_doc);
							rotate_assy_doc_objects (assy_doc, - rotation);
					end case;
			end case;

			move_assy_doc_objects (assy_doc, device.position.place);
		end if;

		result := type_assy_doc (assy_doc);
		return result;
	end get_assy_doc_objects;
	




-- HOLES
	
	function get_holes (
		device_cursor	: in pac_devices_electrical.cursor)
		return pac_holes.list
	is
		holes : pac_holes.list; -- to be returned
		
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;

		rotation : et_board_geometry.type_rotation_model;
	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			holes := get_hole_contours (packge);
					
			case get_face (device_cursor) is
				when TOP =>
					mirror_holes (holes);
					rotate_holes (holes, - rotation);
					
				when BOTTOM =>
					rotate_holes (holes, + rotation);
			end case;
		
			move_holes (holes, device.position.place);
		end if;
		
		return holes;		
	end get_holes;


	

	
	function get_hole_polygons (
		device_cursor	: in pac_devices_electrical.cursor)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		holes : pac_holes.list;
		
		device : type_device_electrical renames element (device_cursor);
		packge : pac_package_models.cursor;
		
		use et_board_geometry.pac_polygons;

		rotation : et_board_geometry.type_rotation_model;
		
		result : pac_polygon_list.list;
	begin
		if is_real (device) then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
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
		end if;
		return result;
	end get_hole_polygons;
	





	function get_terminal (
		device	: in pac_devices_electrical.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return et_terminals.pac_terminals.cursor
	is
		use et_terminals;
		use pac_terminals;

		-- Get the cursor to the full device model in the library:
		use pac_device_models;
		device_model_lib : constant pac_device_models.cursor := get_device_model (device);

		-- This is the name of the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			get_package_variant (device); -- N, D

		-- Get full information about the package variant:
		use pac_package_variants;
		variant_lib : constant pac_package_variants.cursor := 
			get_package_variant (device_model_lib, variant_sch);

		terminal_name : constant pac_terminal_name.bounded_string := 
			get_terminal (variant_lib, unit, port);

		use pac_package_models;
		package_cursor : pac_package_models.cursor;

	begin
		-- Get a cursor to the package model:
		package_cursor := element (variant_lib).model_cursor;
		
		-- Get the cursor to the actual terminal:
		return get_terminal (package_cursor, terminal_name);
	end get_terminal;






	function get_all_terminals (
		device_cursor	: in pac_devices_electrical.cursor) -- IC45
		return pac_terminals.map
	is
		result : pac_terminals.map;
		
		use pac_package_models;
		package_model : pac_package_models.cursor;
	begin
		if is_real (device_cursor) then
			package_model := get_package_model (device_cursor);
			result := element (package_model).terminals;
		end if;
		
		return result;
	end get_all_terminals;

	
	
	
end et_devices_electrical.packages;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
