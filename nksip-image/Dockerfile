# Build stage 0
FROM public.ecr.aws/docker/library/erlang:21

# For apt-get install debian moment
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list \
    && sed -i 's|security.debian.org/debian-security|archive.debian.org/debian-security|g' /etc/apt/sources.list \
    && sed -i '/buster-updates/d' /etc/apt/sources.list

# Install SSH server for debug only
RUN apt-get update && apt-get install -y openssh-server
RUN mkdir /var/run/sshd
RUN useradd -m -s /bin/bash sipuser
RUN echo "sipuser:password" | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN echo "Port 22" >> /etc/ssh/sshd_config
EXPOSE 22

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang app
COPY nksip nksip

# Build app
WORKDIR nksip
RUN make

# Expose ports for SIP
EXPOSE 5060
EXPOSE 5061

CMD ["/bin/bash"]
