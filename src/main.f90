program raytracing
    use vec3_module
    use ray_module
    use sphere_module
    use hit_record_module
    use material_module
    use camera_module

    implicit none

    type(vec3) :: pix_col
    type(sphere), dimension(5) :: world
    type(ray) :: ry
    type(camera) :: cam
    real :: aspect_ratio, u, v, rand_u, rand_v
    integer :: image_height, image_width, x, y, sample, samples_per_pixel

    world = (/                                                                                              &
            sphere(vec3(   0.0, -1000.0,   0.0), 1000.0, material(vec3( 30.0,  30.0,  30.0)/256.0, "LAM")), & 
            sphere(vec3(   0.0,     3.0,  -5.0),    3.0, material(vec3(213.0, 239.0,  85.0)/256.0, "LAM")), & 
            sphere(vec3(  -2.0,     1.0,   0.0),    1.0, material(vec3(190.0, 190.0, 190.0)/256.0, "LAM")), & 
            sphere(vec3(   0.0,     1.0,   0.0),    1.0, material(vec3(135.0, 206.0, 235.0)/256.0, "LAM")), & 
            sphere(vec3(   2.0,     1.0,   0.0),    1.0, material(vec3(253.0, 127.0,  32.0)/256.0, "LAM")) /) 

    ! Image
    aspect_ratio = 256.0 / 160.0 !16.0 / 9.0 ! 256.0 / 160.0
    image_width = 2560
    image_height = int(real(image_width) / aspect_ratio) !2560
    samples_per_pixel = 500

    ! Camera
    cam = init_camera(vec3(0.0,1.0,5.0), vec3(0.0,1.0,0.0), vec3(0.0,1.0,0.0), 60.0, aspect_ratio)

    open(1, file="o.ppm")

    write(1, "(A2)") "P3"
    write(1, "(I5, X, I5)") image_width, image_height
    write(1, "(I3)") 255

    print*, "Writing to file: ", image_width*image_height
    write(*, "(20A)") "--------------------"

    do y=1, image_height
        do x=1, image_width
            pix_col = vec3(0.0, 0.0, 0.0)
            do sample=1, samples_per_pixel

                call random_number(rand_u)
                call random_number(rand_v)

                u = (real(x) + rand_u) / (image_width)
                v = (real(y) + rand_v) / (image_height)

                ry = cam%get_ray(u, v)

                pix_col = pix_col + ray_color(ry, world, size(world), 50)
            end do

            pix_col =  pix_col / real(samples_per_pixel)

            pix_col%x = sqrt(pix_col%x)
            pix_col%y = sqrt(pix_col%y)
            pix_col%z = sqrt(pix_col%z)

            pix_col = clamp(0.999, pix_col) * 256.0

            write(1, "(I3, X, I3, X, I3, /)", advance="no") &
                int(pix_col%x), int(pix_col%y), int(pix_col%z)

            if (mod(y*image_width + x, int(image_height*image_width*0.05)) .eq. 0) then
                write(*, "(A)", advance="no") "="
            end if
        end do
    end do

    print*

    close(1)

contains

    function hit_sphere(r, s, t_min, t_max, hit_rec)
        type(ray), intent(in) :: r
        type(sphere), intent(in) :: s
        real, intent(in) :: t_min, t_max
        type(hit_record), intent(out) :: hit_rec
        logical :: hit_sphere

        type(vec3) :: oc, outward_normal
        real :: a, b_half, c, discriminant, sqr_disc, root

        oc = r%origin - s%center
        a = length_squared(r%direction)
        b_half = dot(oc, r%direction)
        c = length_squared(oc) - s%radius**2

        discriminant = b_half**2 - a*c

        if (discriminant .lt. 0.0) then
            hit_sphere = .false.
            return
        end if

        sqr_disc = sqrt(discriminant)

        root = (-b_half - sqr_disc) / a
        if ((root .lt. t_min) .or. (t_max .lt. root)) then
            root = (-b_half + sqr_disc) / a

            if ((root .lt. t_min) .or. (t_max .lt. root)) then
                hit_sphere = .false.
                return
            end if
        end if

        hit_rec%t = root
        hit_rec%p = at(r, hit_rec%t)
        outward_normal = (hit_rec%p - s%center) / s%radius
        hit_rec%mat = s%mat

        call set_face_normal(hit_rec, r, outward_normal)

        hit_sphere = .true.

    end function

    function hit_list(r, list, list_len, t_min, t_max, hit_rec)
        type(ray), intent(in) :: r
        type(sphere), intent(in) :: list(*)
        real, intent(in) :: t_min, t_max
        type(hit_record), intent(out) :: hit_rec
        integer, intent(in) :: list_len
        logical :: hit_list
        
        type(hit_record) :: temp_rec
        real :: closest
        integer :: i

        hit_list = .false.
        closest = t_max

        do i=1, list_len
            if (hit_sphere(r, list(i), t_min, closest, temp_rec)) then
                hit_list = .true.
                closest = temp_rec%t
                hit_rec = temp_rec
            end if
        end do

    end function hit_list

    function lambertian_scatter(hit_rec, attenuation, scattered)
        type(hit_record), intent(in) :: hit_rec
        type(vec3), intent(out) :: attenuation
        type(ray), intent(out) :: scattered
        logical :: lambertian_scatter

        type(vec3) :: scatter_direction

        scatter_direction = hit_rec%normal + random_unit_vector()

        if (near_zero(scatter_direction)) then
            scatter_direction = hit_rec%normal
        end if

        scattered = ray(hit_rec%p, scatter_direction)
        attenuation = hit_rec%mat%albedo

        lambertian_scatter = .true.
    end function lambertian_scatter

    function metal_scatter(r, hit_rec, attenuation, scattered)
        type(ray), intent(in) :: r
        type(hit_record), intent(in) :: hit_rec
        type(vec3), intent(out) :: attenuation
        type(ray), intent(out) :: scattered
        logical :: metal_scatter

        type(vec3) :: reflected

        reflected = reflect(unit_vector(r%direction), hit_rec%normal)

        scattered = ray(hit_rec%p, reflected)
        attenuation = hit_rec%mat%albedo

        metal_scatter = dot(scattered%direction, hit_rec%normal) .gt. 0.0
    end function metal_scatter

    recursive function ray_color(r, list, list_len, depth) result(r_color)
        type(ray), intent(in) :: r
        type(sphere), intent(in) :: list(*)
        integer, intent(in) :: list_len, depth

        type(vec3) :: r_color, unit_direction, attenuation
        type(ray) :: scattered
        real :: t
        type(hit_record) :: hit_rec

        if (depth .le. 0) then
            r_color = vec3(0.0, 0.0, 0.0)
            return
        end if

        if (hit_list(r, list, list_len, 0.001, 3.40282347E+38, hit_rec)) then !3.40282347E+38

            if (hit_rec%mat%mat_type .eq. "LAM") then
                if (lambertian_scatter(hit_rec, attenuation, scattered)) then
                    r_color = attenuation * ray_color(scattered, list, list_len, depth-1)
                    return
                end if
            else if (hit_rec%mat%mat_type .eq. "MET") then
                if (metal_scatter(r, hit_rec, attenuation, scattered)) then
                    r_color = attenuation * ray_color(scattered, list, list_len, depth-1)
                    return
                end if
            else
                r_color = vec3(0.0, 0.0, 0.0)
            end if
        end if

        unit_direction = unit_vector(r%direction)
        t = 0.5 * (unit_direction%y + 1.0)
        r_color = vec3(1.0, 1.0, 1.0)*(t) + vec3(0.5, 0.7, 1.0)*(1-t)

    end function

end program raytracing