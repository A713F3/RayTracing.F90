program raytracing
    use vec3_module
    use ray_module
    use sphere_module
    use ppm

    implicit none

    type(vec3) :: pix_col, origin, lower_left_corner
    type(vec3) ::  horizontal, vertical
    type(sphere) :: sph
    type(ray) :: ry
    real :: aspect_ratio, viewport_height, viewport_width
    real :: focal_length, u, v
    integer :: image_height, image_width, x, y, s, samples_per_pixel

    sph = sphere(vec3(0.0, 0.0, -1.0), 0.5)

    ! Image
    aspect_ratio = 16.0 / 9.0
    image_height = 700
    image_width = int(aspect_ratio * real(image_height))
    samples_per_pixel = 100

    ! Camera
    viewport_height = 2.0
    viewport_width = aspect_ratio * viewport_height
    focal_length = 1.0

    origin = vec3(0.0, 0.0, 0.0)
    horizontal = vec3(viewport_width, 0.0, 0.0)
    vertical = vec3(0.0, viewport_height, 0.0)
    lower_left_corner = origin - horizontal/2.0 - vertical/2.0 - vec3(0, 0, focal_length)

    call ppm_init(image_width, image_height)

    do y=1, image_height
        do x=1, image_width
            pix_col = vec3(0.0, 0.0, 0.0)
            do s=1, samples_per_pixel

                u = (real(x) + rand(0)) / (image_width)
                v = (real(y) + rand(0)) / (image_height)
                ry = ray(origin, lower_left_corner + horizontal*u + vertical*v - origin);

                pix_col = pix_col + ray_color(ry, sph)
            end do

            pix_col = pix_col * 255.9999 / real(samples_per_pixel)

            call point(x, y, color(int(pix_col%x), int(pix_col%y), int(pix_col%z)))
        end do
    end do

    call render_image(1, "o", .true.)

    call ppm_release()

contains
    function hit_sphere(r, s)
        type(ray), intent(in) :: r
        type(sphere), intent(in) :: s
        real :: hit_sphere

        type(vec3) :: oc
        real :: a, b, c, discriminant

        oc = r%origin - s%center
        a = dot(r%direction, r%direction)
        b = 2.0 * dot(oc, r%direction)
        c = dot(oc, oc) - s%raidus**2
        discriminant = b**2 - 4*a*c

        hit_sphere = -1.0

        if (discriminant .ge. 0.0) then
            hit_sphere = (-b - sqrt(discriminant) ) / (2.0*a)
        end if

        ! hit_sphere = discriminant .gt. 0.0

    end function

    function ray_color(r, s)
        type(ray), intent(in) :: r
        type(sphere), intent(in) :: s

        type(vec3) :: ray_color, unit_direction, N
        real :: t

        t = hit_sphere(r, s)

        if (t .gt. 0.0) then
            N = unit_vector(at(r, t) - vec3(0.0, 0.0, -1.0))
            ray_color = vec3(N%x + 1.0, N%y + 1.0, N%z + 1.0) * 0.5
        else    
            unit_direction = unit_vector(r%direction)
            t = 0.5 * (unit_direction%y + 1.0)
            ray_color = vec3(1.0, 1.0, 1.0)*(t) + vec3(0.5, 0.7, 1.0)*(1-t)
        end if

    end function

end program raytracing