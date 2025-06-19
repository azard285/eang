# Build stage 0
FROM erlang:21

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

# Copy Erlang application
COPY nksip_samples nksip_samples

# Build
WORKDIR nksip_samples
#RUN make pbx

EXPOSE 5060
EXPOSE 5061

CMD ["/bin/bash"]
